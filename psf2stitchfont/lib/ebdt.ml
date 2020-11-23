[%%cstruct
  type header = {
    major_version : uint16_t;
    minor_version : uint16_t;
  } [@@big_endian]]

(* we'll assume image format 5 for this *)
(* oooooooof this inconsistent-ass naming yikes *)
let glyph_data_to_pattern ~width ~height ~offset ebdt glyph_ids =
  let n_bytes = (Rawbytes.pad_to_8 (width * height * glyph_ids)) / 8 in
  let data = Cstruct.sub ebdt offset n_bytes in
  let coordinates = Rawbits.glyphs_of_bytes ~height ~width data in
  List.map (fun stitches -> {Stitchy.Types.stitches; width; height }) coordinates
