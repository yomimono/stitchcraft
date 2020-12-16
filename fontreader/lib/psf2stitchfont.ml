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

type error =
  [ `Too_short
  | `Wrong_magic of Cstruct.t
  | `No_unicode
  | `No_unicode_info ]

type glyphmap = Stitchy.Types.glyph list * Uchar.t list list

let psf2magic = Cstruct.of_string "\x72\xb5\x4a\x86"

let pp_error fmt = function
  | `Too_short -> Format.fprintf fmt "file too small to contain valid psf2 header"
  | `Wrong_magic c -> Format.fprintf fmt "file is not a valid psf2 file (wrong magic: %a)" Cstruct.hexdump_pp c
  | `No_unicode -> Format.fprintf fmt "file is not unicode-ready according to psf2 header flags"
  | `No_unicode_info -> Format.fprintf fmt "file contains no unicode information"

let parse_glyph_table ~width ~height table =
  let bytes_in_row = Rawbytes.bytes_in_row width in
  let rec next_glyph glyphs table =
    if Cstruct.len table < bytes_in_row * height then glyphs
    else begin
      let stitches = Rawbytes.read_rows table ~bytes_in_row ~height ~width [] 0 in
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
    | `Uchar u -> try_chunk (u::l)
    | `Await | `End -> l
    | `Malformed _ -> l (* this looks scary, but is the normal case :) *)
  in
  let chars = try_chunk [] in
  (* tell us how far you got! *)
  let cursor = Uutf.decoder_byte_count decoder in
  if cursor >= Cstruct.len table then (chars, `End)
  else (chars, `More_at cursor)

let keep_getting_glyphs table =
  let rec aux t so_far =
    match next_unicode_entry t with
    | l, `End -> List.rev (l :: so_far)
    | l, `More_at n -> aux (Cstruct.shift t n) (l::so_far)
  in
  aux table []

let read_unicode_glyphmap buffer =
  let glyphs_size = Int32.mul (get_psf2header_number_of_glyphs buffer) (get_psf2header_bytes_per_character buffer) in
  let unicode_start = Int32.add (get_psf2header_start_of_glyphs buffer) glyphs_size in
  let unicode_length = (Cstruct.len buffer) - (Int32.to_int unicode_start) in
  let glyph_table = Cstruct.sub buffer (Int32.to_int (get_psf2header_start_of_glyphs buffer)) (Int32.to_int glyphs_size) in
  let unicode_table = Cstruct.shift buffer (Int32.to_int unicode_start) in
  if unicode_length < 0 then Error `No_unicode_info
  else begin
    let glyphs = List.rev @@ parse_glyph_table
        ~width:(get_psf2header_width buffer |> Int32.to_int)
        ~height:(get_psf2header_height buffer |> Int32.to_int) glyph_table in
    let unicode_map = keep_getting_glyphs unicode_table in
    Ok (glyphs, unicode_map)
  end

let read_nonunicode_glyphmap buffer =
  let number_of_glyphs = get_psf2header_number_of_glyphs buffer in
  let glyphs_size = Int32.mul number_of_glyphs (get_psf2header_bytes_per_character buffer) in
  let glyph_table = Cstruct.sub buffer (Int32.to_int (get_psf2header_start_of_glyphs buffer)) (Int32.to_int glyphs_size) in
  let width = get_psf2header_width buffer |> Int32.to_int
  and height = get_psf2header_height buffer |> Int32.to_int
  in
  let glyphs = parse_glyph_table ~width ~height glyph_table |> List.rev in
  let unicode_map = List.init (Int32.to_int number_of_glyphs) (fun n -> [Uchar.of_int n]) in
  Ok (glyphs, unicode_map)

let glyphmap_of_buffer buffer =
  if Cstruct.len buffer < 0x20 then Error `Too_short
  else if 0 <> Cstruct.compare psf2magic (get_psf2header_magic buffer)
  then Error (`Wrong_magic (get_psf2header_magic buffer))
  else if get_psf2header_flags buffer <> 0x01l
  then read_nonunicode_glyphmap buffer
  else read_unicode_glyphmap buffer
