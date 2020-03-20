[%%cstruct
  type psf2header = {
    magic : uint8_t [@len 4];
    version : uint32_t;
    start_of_glyphs : uint32_t;
    flags : uint32_t;
    number_of_glyphs : uint32_t;
    bytes_per_character : uint32_t;
    height : uint32_t; (* in bits *)
    width : uint32_t; (* also in bits *)
  } [@@little_endian]]

let psf2magic = Cstruct.of_string "\x72\xb5\x4a\x86"

let pp_error fmt = function
  | `Too_short -> Format.fprintf fmt "file too small to contain valid psf2 header"
  | `Wrong_magic c -> Format.fprintf fmt "file is not a valid psf2 file (wrong magic: %a)" Cstruct.hexdump_pp c
  | `No_unicode -> Format.fprintf fmt "file is not unicode-ready according to psf2 header flags"
  | `No_unicode_info -> Format.fprintf fmt "file contains no unicode information"

(* if height and weight aren't evenly divisible by 8,
   they'll be padded to fit (probably with 0s, since this is a bitmap font) *)
let pad_to_8 n =
  if n mod 8 = 0 then n
  else n + (8 - (n mod 8))

let high_bit_set byte = byte land 0x80 = 0x80

let to_bit_list byte =
  let rec aux l index byte = match (int_of_char byte) land 0xff with
    | 0 -> l
    | nonzero when high_bit_set nonzero ->
      let byte = (int_of_char byte) lsl 1 |> (land) 0xff |> char_of_int in
      aux (index :: l)  (index + 1) byte
    | _unset_bit ->
      let byte = (int_of_char byte) lsl 1 |> (land) 0xff |> char_of_int in
      aux l (index + 1) byte
  in
  aux [] 0 byte

let read_row ~y row ~width =
  let rec aux bits row index =
    if index >= width || Cstruct.len row = 0 then bits
    else begin
      let bit_list = to_bit_list (Cstruct.get_char row 0) in
      let new_bits = List.map (fun bit_number -> (bit_number + 8*index, y)) bit_list in
      aux (new_bits @ bits) (Cstruct.shift row 1) (index + 1)
    end
  in
  let stitches = aux [] row 0 in
  stitches

let rec read_rows buffer ~bytes_in_row ~height ~width rows index =
  if index >= height then rows
  else begin
    let row = read_row ~y:index (Cstruct.sub buffer 0 bytes_in_row) ~width in
    read_rows (Cstruct.shift buffer bytes_in_row) ~bytes_in_row ~height ~width (row @ rows) (index + 1)
  end

let parse_glyph_table ~width ~height table =
  let bytes_in_row = width / 8 in
  let rec next_glyph glyphs table =
    if Cstruct.len table < bytes_in_row * height then glyphs
    else begin
      let stitches = read_rows table ~bytes_in_row ~height ~width [] 0 in
      let glyph : Stitchy.Types.glyph = {
        stitches;
        height;
        width;
      } in
      next_glyph (glyph :: glyphs) (Cstruct.shift table (bytes_in_row * height))
    end
  in
  next_glyph [] table

(* the strategy seems to be to read utf-8 unicode strings until we can't anymore,
   then look to see whether the next byte is `fe` (start of sequence)
   or `ff` (end of this unicode table entry) *)
(* returns the uchars in question and the place to look for a next one *)
let next_unicode_entry table =
  let s = Cstruct.to_string table in
  let decoder = Uutf.decoder ~encoding:(`UTF_8) (`String s) in
  let rec try_chunk l =
    match Uutf.decode decoder with
    | `Uchar u -> Format.printf "ok got a char %a yay\n%!" Fmt.Dump.uchar u; try_chunk (u::l)
    | `Malformed e -> Format.printf "malformed string: %s\n%!" e; l
    | `Await -> Format.printf "await, wtf?\n%!"; l
    | `End -> Format.printf "normal end to utf-8 string, found %d uchars\n%!" (List.length l); l
  in
  let chars = try_chunk [] in
  (* tell us how far you got! *)
  let cursor = Uutf.decoder_byte_count decoder in
  Format.printf "%d (0x%x) bytes decoded out of %d (%x)\n%!" cursor cursor (Cstruct.len table) (Cstruct.len table);
  if cursor >= Cstruct.len table then (chars, `End)
  else begin
    match String.get s cursor |> int_of_char with
    | 0xfe -> (* for now let's punt *)
      let end_of_sequence = String.index_from s cursor (char_of_int 0xff) in
      (chars, `More_at (end_of_sequence + 1))
    | 0xff -> (* end of thing, no sequence *) (chars, `More_at (cursor + 1))
    | _ -> (chars, `More_at cursor)
  end

let keep_getting_glyphs table =
  let rec aux t so_far =
    match next_unicode_entry t with
    | l, `End -> List.rev (l :: so_far)
    | l, `More_at n -> aux (Cstruct.shift t n) (l::so_far)
  in
  aux table []

let glyphmap_of_psf_header buffer =
  if Cstruct.len buffer < 0x20 then Error `Too_short
  else if 0 <> Cstruct.compare psf2magic (get_psf2header_magic buffer) then Error (`Wrong_magic (get_psf2header_magic buffer))
  else if get_psf2header_flags buffer <> 0x01l then Error `No_unicode (* TODO: we can probably do something else here *)
  else begin
    let glyphs_size = Int32.mul (get_psf2header_number_of_glyphs buffer) (get_psf2header_bytes_per_character buffer) in
    let unicode_start = Int32.add (get_psf2header_start_of_glyphs buffer) glyphs_size in
    let unicode_length = (Cstruct.len buffer) - (Int32.to_int unicode_start) in
    let glyph_table = Cstruct.sub buffer (Int32.to_int (get_psf2header_start_of_glyphs buffer)) (Int32.to_int glyphs_size) in
    let unicode_table = Cstruct.shift buffer (Int32.to_int unicode_start) in
    if unicode_length < 0 then Error `No_unicode_info
    else begin
      let glyphs = parse_glyph_table
          ~width:(get_psf2header_width buffer |> Int32.to_int)
          ~height:(get_psf2header_height buffer |> Int32.to_int) glyph_table in
      let unicode_map = keep_getting_glyphs unicode_table in
      Ok (`Glyphmap (glyphs, unicode_map))
    end
  end
