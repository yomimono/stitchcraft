open Angstrom

type pattern = {
  version : int;
  width : int;
  height : int;
  cloth_count_width : int;
  cloth_count_height : int;
  fabric_color : int * int * int * int;
}

type metadata = {
  author : string option;
  copyright : string option;
  title : string option;
  fabric : string option;
  instructions : string option;
  keywords : string option;
  website : string option;
}

type palette_entry = {
  scheme : string;
  color_name : string;
  color_description : string;
  color : int * int * int * int;
  symbol_font : string;
  stitch_bytes : int list; (* TODO this should be an enum if it's actually useful. *)
}

type palette = palette_entry list

let pp_dechex fmt n =
  Format.fprintf fmt "%d (0x%x)" n n

let pp_rgba fmt (r, g, b, a) =
  Format.fprintf fmt "R%d G%d B%d A%d (#%02x%02x%02x%02x)" r g b a r g b a

let pp_palette_entry fmt e =
  Format.fprintf fmt "scheme %s, color name %s, description %s, color %a"
    e.scheme e.color_name e.color_description pp_rgba e.color

let pp_palette = Fmt.list pp_palette_entry

let pp_fabric fmt fabric =
  Format.fprintf fmt "version %d; pattern size %a by %a; cloth count %a by %a (per inch); fabric color %a"
    fabric.version
    pp_dechex fabric.width
    pp_dechex fabric.height
    pp_dechex fabric.cloth_count_width
    pp_dechex fabric.cloth_count_height
    pp_rgba fabric.fabric_color

let pp_metadata fmt metadata =
  Format.fprintf fmt "author: %a\n" Fmt.(option string) metadata.author;
  Format.fprintf fmt "copyright: %a\n" Fmt.(option string) metadata.copyright;
  Format.fprintf fmt "title: %a\n" Fmt.(option string) metadata.title;
  Format.fprintf fmt "fabric: %a\n" Fmt.(option string) metadata.fabric;
  Format.fprintf fmt "instructions: %a\n" Fmt.(option string) metadata.instructions;
  Format.fprintf fmt "keywords: %a\n" Fmt.(option string) metadata.keywords

let magic = string "PCStitch "
let version = any_char
let more_magic = string " Pattern File"
let skip_spaces = advance (0x100 - String.length("PCStitch 7 Pattern File"))

let header =
  (magic *> version) <* more_magic
   <?>
   "file header incorrect -- is this a .pat file?"

let rgba =
  any_uint8 >>= fun r ->
  any_uint8 >>= fun g ->
  any_uint8 >>= fun b ->
  any_uint8 >>= fun a ->
  return (r, g, b, a)

let len_and_str =
  LE.any_uint16 >>= fun length ->
  take length

module V7 = struct
  let width = LE.any_uint16
  and height = LE.any_uint16
  and cloth_count_height = LE.any_uint16
  and cloth_count_width = LE.any_uint16

  let known_postheader =
    skip_spaces *> (* advance to position 0x100 *)
    LE.any_int32 >>= fun _unknown ->
    LE.any_int32 >>= fun _unknown ->
    LE.any_uint16 >>= fun width ->
    LE.any_uint16 >>= fun height ->
    LE.any_uint16 >>= fun cloth_count_width -> (* e.g., 14x14 for 14-count Aida *)
    LE.any_uint16 >>= fun cloth_count_height ->
    rgba >>= fun fabric_color ->
    return {version = 7;
            width; height; cloth_count_width; cloth_count_height; fabric_color}

  let metadata =
    (* call this at the beginning of the variable length metadata stuff, which is at 0x114 .
     * that's after known_postheader *)
    len_and_str >>= fun author ->
    len_and_str >>= fun copyright ->
    len_and_str >>= fun title ->
    len_and_str >>= fun fabric ->
    len_and_str >>= fun instructions ->
    len_and_str >>= fun keywords ->
    len_and_str >>= fun website ->
    let maybe s = if String.length s > 0 then Some s else None in
    return { author = maybe author;
             copyright = maybe copyright;
             title = maybe title;
             fabric = maybe fabric;
             instructions = maybe instructions;
             keywords = maybe keywords;
             website = maybe website;
    }

  let palette_entry =
    take 33 >>= fun scheme ->
    take 10 >>= fun color_name ->
    take 30 >>= fun color_description ->
    take 4 >>= fun _unknown1 ->
    rgba >>= fun color ->
    take 81 >>= fun _unknown2 ->
    take 30 >>= fun symbol_font ->
    take 7 >>= fun _unknown3 ->
    take 30 >>= fun _repeated_color_description ->
    rgba >>= fun _repeated_rgba ->
    take 10 >>= fun _repeated_color_name ->
    count 7 any_uint8 >>= fun stitch_bytes ->
    return {scheme; color_name; color_description; color; symbol_font; stitch_bytes }
    
  let palette =
    (* we expect this to start right after the end of the "website" metadata. *)
    advance 4 *>
    string "PCStitch Floss Palette!!!" *>
    advance 4 *>
    LE.any_uint16 >>= fun num_palette_entries ->
    count num_palette_entries palette_entry

  let stitch =
    LE.any_uint16 >>= fun n_stitches ->
    any_uint8 >>= fun stitch_color_index -> (* nb this is 1-indexed :eyeroll: *)
    any_uint8 >>= fun stitch_type ->
    return (n_stitches, stitch_color_index, stitch_type)

  let stitches width height =
    let expected = width * height in

end

let file =
  header >>= function
  | '7' ->
    V7.known_postheader >>= fun fabric ->
    V7.metadata >>= fun metadata ->
    V7.palette >>= fun palette ->
    let n_stitches = fabric.width * fabric.height in
  | _ -> fail "unknown file format -- try a version 7 file"
