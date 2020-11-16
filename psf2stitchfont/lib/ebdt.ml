[%%cstruct
  type header = {
    major_version : uint16_t;
    minor_version : uint16_t;
  } [@@big_endian]]

(* we'll assume image format 5 for this *)
let glyph_data_to_pattern ebdt ~width ~height ~size ~offset =
  let glyph = Cstruct.sub ebdt offset size in
  let bytes_in_row = (width / 8) + (if width mod 8 != 0 then 1 else 0) in
  let stitches = Rawbytes.read_rows glyph ~bytes_in_row ~height ~width [] 0 in
  let stitchy_glyph : Stitchy.Types.glyph = {
    stitches;
    height;
    width;
  } in
  stitchy_glyph
