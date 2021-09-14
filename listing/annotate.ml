let pdf_glyph_dims = (5, 7)

let add_pdf image ~orientation (r, g, b) =
  (* need 5 vertical pixels and 3 horizontal for each glyph *)
  (* this is *ludicrously* bad internationalization,
     but I'm assuming that .pdf is a global idea...? *)
  let p_glyph = [
    (0, 0); (1, 0); (2, 0);
    (0, 1);         (2, 1);
    (0, 2); (1, 2); (2, 2);
    (0, 3);
    (0, 4);
  ]
  and d_glyph = [
    (0, 0); (1, 0);
    (0, 1);         (2, 1);
    (0, 2);         (2, 2);
    (0, 3);         (2, 3);
    (0, 4); (1, 4);
  ]
  and f_glyph = [
    (0, 0); (1, 0); (2, 0);
    (0, 1);
    (0, 2); (1, 2);
    (0, 3);
    (0, 4);
  ]
  and bang = [
    (1, 0);
    (1, 1);
    (1, 2);

    (1, 4);
  ]
  and glyph_width, glyph_height = pdf_glyph_dims
  in
  let glyphs = [p_glyph; d_glyph; f_glyph; bang] in
  let write ~start_x ~start_y ~orientation glyphs =
    let translate_to_image_coordinates nth (x, y) = match orientation with
      | `Horizontal _ -> ((start_x + x + nth * glyph_width), (y + start_y))
      | `Vertical _ -> ((start_x + x), (nth * glyph_height) + (start_y + y))
    in
    List.iteri (fun nth glyph ->
        List.iter (fun coords ->
            let (x, y) = translate_to_image_coordinates nth coords in
            Image.write_rgb image x y r g b)
          glyph
      ) glyphs
  in
  match orientation with
  | `Horizontal top_edge ->
    (* center the text horizontally *)
    let start_x = (image.width / 2) - (((List.length glyphs) / 2) * glyph_width)
    (* vertically, make sure it's above the original image *)
    and start_y = max 0 (top_edge / 2 - glyph_height)
    in
    write ~start_x ~start_y ~orientation glyphs
  | `Vertical left_edge ->
    (* make sure the text is to the left of the original image *)
    let start_x = max 0 (left_edge / 2 - glyph_width)
    (* center the text vertically *)
    and start_y = (image.height / 2) - (((List.length glyphs) / 2) * glyph_height)
    in
    write ~start_x ~start_y ~orientation glyphs
