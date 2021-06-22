open Types

let thick_line_thickness = 1.
let thin_line_thickness = 0.5

let paint_grid_lines (doc : doc) (page : page) =
  let thickness n = if n mod doc.fat_line_interval = 0 then thick_line_thickness else thin_line_thickness in
  let paint_line ~thickness (x1, y1) (x2, y2) =
    Pdfops.([
        Op_q;
        Op_w thickness;
        Op_m (x1, y1);
        Op_l (x2, y2);
        Op_s;
        Op_Q;
      ])
  in
  let paint_horizontal_line y =
    let (left_pt_x, y_pt) = Positioning.find_upper_left doc 0 y in
    let (right_pt_x, _) = Positioning.find_upper_left doc (snd page.x_range - fst page.x_range) y in
    paint_line ~thickness:(thickness (y + fst page.y_range)) (left_pt_x, y_pt) (right_pt_x, y_pt)
  in
  let paint_vertical_line x =
    let (x_pt, top_y_pt) = Positioning.find_upper_left doc x 0 in
    let (_, bottom_y_pt) = Positioning.find_upper_left doc x (snd page.y_range - fst page.y_range) in
    paint_line ~thickness:(thickness (x + fst page.x_range)) (x_pt, top_y_pt) (x_pt, bottom_y_pt)
  in
  let rec paint_horizontal_lines n l =
    let n_lines = (snd page.y_range) - (fst page.y_range) in
    if n > n_lines then l
    else (paint_horizontal_lines (n+1) (paint_horizontal_line n @ l))
  in
  let rec paint_vertical_lines n l =
    let n_lines = (snd page.x_range) - (fst page.x_range) in
    if n > n_lines then l
    else (paint_vertical_lines (n+1) (paint_vertical_line n @ l))
  in
  paint_horizontal_lines 0 [] @ paint_vertical_lines 0 []

let make_grid_label ~text_size n n_x_pos n_y_pos =
  Pdfops.([
      Op_q;
      Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (n_x_pos, n_y_pos)]);
      Op_Tf (Consts.helvetica_key, text_size);
      Op_BT; (* begin text object *)
      Op_Tj (string_of_int n); (* actual label *)
      Op_ET; (* done! *)
      Op_Q;
    ])

let label_top_line doc page n =
  (* label n goes where the nth pixel would go, just nudged up a bit *)
  let (n_x_pos, n_y_pos) = Positioning.find_upper_left doc (n - (fst page.x_range)) 0 in
  make_grid_label ~text_size:(float_of_int doc.pixel_size) n n_x_pos (n_y_pos +. (float_of_int doc.pixel_size))

let label_left_line doc page n =
  (* label n goes where the nth pixel would go, just nudged left a bit *)
  (* TODO: it would be good to right-justify this; it's weird-looking with different-width labels *)
  (* is there a way to right-justify this bad boy? *)
  let (n_x_pos, n_y_pos) = Positioning.find_upper_left doc 0 (n - (fst page.y_range)) in
  make_grid_label ~text_size:(float_of_int doc.pixel_size) n (n_x_pos -. (float_of_int (doc.pixel_size * 2))) n_y_pos

let label_top_grid doc page =
  let rec label n l =
    if n > snd page.x_range then l
    else
      label (n+doc.fat_line_interval) (label_top_line doc page n @ l) 
  in
  (* find the first grid line that is evenly divisible by fat_line_interval; start there *)
  match (fst page.x_range) mod doc.fat_line_interval with
  | 0 -> label (fst page.x_range) []
  | _ -> label (((fst page.x_range / doc.fat_line_interval) + 1) * doc.fat_line_interval) []

let label_left_grid doc page =
  let rec label n l =
    if n > snd page.y_range then l
    else
      label (n+doc.fat_line_interval) (label_left_line doc page n @ l) 
  in
  (* find the first grid line that is evenly divisible by fat_line_interval; start there *)
  match (fst page.y_range) mod doc.fat_line_interval with
  | 0 -> label (fst page.y_range) []
  | _ -> label (((fst page.y_range / doc.fat_line_interval) + 1) * doc.fat_line_interval) []
