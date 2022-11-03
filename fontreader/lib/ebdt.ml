[%%cstruct
  type header = {
    major_version : uint16_t;
    minor_version : uint16_t;
  } [@@big_endian]]

(* we'll assume image format 5 for this *)
let unicode_glyph_data_to_stitchy_glyph debug ~width ~height ~offset ebdt n_glyphs =
  let size_in_bytes = (width * height / 8) + if width * height mod 8 > 0 then 1 else 0 in
  let data = Cstruct.sub ebdt offset (n_glyphs * size_in_bytes) in
  if debug then Printf.printf "for %d glyphs, got %d bytes starting at offset %x\n%!" n_glyphs (Cstruct.length data) offset;
  let coordinates = Rawbits.glyphs_of_bytes debug ~height ~width data in
  let open Stitchy.Types in
  List.map (fun stitches ->
      {stitches = CoordinateSet.of_list stitches; width; height }
    ) coordinates
