(* both annotation options ("kit!" "pdf!") use glyphs
 * of the same dimensions and have a '!', so share those aspects *)
let bang = [
    (1, 0);
    (1, 1);
    (1, 2);

    (1, 4);
  ]

let glyph_width, glyph_height = (5, 7)
module Pdf = struct
  let p = [
    (0, 0); (1, 0); (2, 0);
    (0, 1);         (2, 1);
    (0, 2); (1, 2); (2, 2);
    (0, 3);
    (0, 4);
  ]
  and d = [
    (0, 0); (1, 0);
    (0, 1);         (2, 1);
    (0, 2);         (2, 2);
    (0, 3);         (2, 3);
    (0, 4); (1, 4);
  ]
  and f = [
    (0, 0); (1, 0); (2, 0);
    (0, 1);
    (0, 2); (1, 2);
    (0, 3);
    (0, 4);
  ]
  let glyphs = [p; d; f; bang]
end

module Kit = struct
  let k = [
    (0, 0);         (2, 0);
    (0, 1);         (2, 1);
    (0, 2); (1, 2);
    (0, 3);         (2, 3);
    (0, 4);         (2, 4);
  ]
  and i = [
            (1, 0);
            (1, 1);
            (1, 2);
            (1, 3);
            (1, 4);
  ]
  and t = [
    (0, 0); (1, 0); (2, 0);
            (1, 1);
            (1, 2);
            (1, 3);
            (1, 4);
  ]
  let glyphs = [k; i; t; bang]
end

let glyphs_of_annotation = function
  | `Kit -> Kit.glyphs
  | `Pdf -> Pdf.glyphs

(*  since "background" already means something else,
 *  reuse a term from framing for the extra stuff we're sticking
 *  in to make the thing fit a particular aspect ratio. *)
let color_matting image orientation color =
  let paint_rectangle image (r, g, b) ~start_x ~start_y ~last_x ~last_y =
    for row = start_y to last_y do
      for column = start_x to last_x do
        Image.write_rgb image column row r g b
      done
    done
  in
  let fill = paint_rectangle image color in
  let image_max_x, image_max_y = (image.Image.width - 1, image.Image.height - 1) in
  match orientation with
  | `Vertical (left_edge, right_edge) ->
    (* write pixels from (0, 0) through (left_edge, image.max) and (right_edge, 0) through (image.max, image.max) *)
    fill ~start_x:0 ~start_y:0 ~last_x:left_edge ~last_y:image_max_y;
    fill ~start_x:right_edge ~start_y:0 ~last_x:image_max_x ~last_y:image_max_y
  | `Horizontal (top_edge, bottom_edge) ->
    (* write pixels from (0, 0) through (image.max, top_edge) and (bottom_edge, 0) through (image.max, image.max) *)
    fill ~start_x:0 ~start_y:0 ~last_x:image_max_x ~last_y:top_edge;
    fill ~start_x:0 ~start_y:bottom_edge ~last_x:image_max_x ~last_y:image_max_y

let write image glyphs orientation (r, g, b) ~start_x ~start_y =
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

let add_pdf image ~annotation ~orientation ~text_color ~matting_color =
  (* need 5 vertical pixels and 3 horizontal for each glyph *)
  (* this is *ludicrously* bad internationalization,
     but I'm assuming that .pdf is a global idea...? *)
  let glyphs = glyphs_of_annotation annotation in
  let start_x, start_y = match orientation with
  | `Horizontal (top_edge, _) ->
    (* center the text horizontally *)
    let start_x = (image.Image.width / 2) - (((List.length glyphs) / 2) * glyph_width)
    (* vertically, make sure it's above the original image *)
    and start_y = max 0 (top_edge / 2 - glyph_height)
    in
    (start_x, start_y)
  | `Vertical (left_edge, _) ->
    (* make sure the text is to the left of the original image *)
    let start_x = max 0 (left_edge / 2 - glyph_width)
    (* center the text vertically *)
    and start_y = (image.Image.height / 2) - (((List.length glyphs) / 2) * glyph_height)
    in
    (start_x, start_y)
  in
  color_matting image orientation matting_color;
  write image glyphs orientation text_color ~start_x ~start_y
