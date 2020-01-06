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
  and width, height = pdf_glyph_dims
  in
  let glyphs = [p_glyph; d_glyph; f_glyph; bang] in
  let write ~start_x ~start_y ~orientation glyphs =
    List.iteri (fun nth glyph ->
        List.iter (fun (x, y) ->
            let (x, y) = match orientation with
              | `Horizontal _ -> ((start_x + x + nth * width), (y + start_y))
              | `Vertical _ -> ((start_x + x), (nth * height) + (start_y + y))
            in
            Image.write_rgb image x y r g b)
          glyph
      ) glyphs
  in
  match orientation with
  | `Horizontal orig_min_y ->
    let start_x = (image.width / 2) - (((List.length glyphs) / 2) * width)
    and start_y = (image.height - orig_min_y) / 2
    in
    write ~start_x ~start_y ~orientation glyphs
  | `Vertical orig_min_x ->
    let start_x = (image.width - orig_min_x) / 2
    and start_y = (image.height / 2) - (((List.length glyphs) / 2) * height)
    in
    write ~start_x ~start_y ~orientation glyphs
