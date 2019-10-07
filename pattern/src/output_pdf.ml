(* at toplevel since used by both pixel-painter and gridline-painter *)
(* half-inch margins *)
let base_unit = 72.
let max_x = base_unit *. 8. (* default user scale is 1/72nd of an inch; us_letter is 8.5 inches wide *)
let max_y = base_unit *. 10.5 (* same idea for y *)
let min_x = base_unit *. 0.5
let min_y = base_unit *. 0.5

let symbol_font =
  Pdf.(Dictionary
         [("/Type", Name "/Font");
          ("/Subtype", Name "/Type1");
          ("/BaseFont", Name "/Symbol");])

let zapf_font = 
  Pdf.(Dictionary
         [("/Type", Name "/Font");
          ("/Subtype", Name "/Type1");
          ("/BaseFont", Name "/ZapfDingbats");])

let helvetica_font = 
  Pdf.(Dictionary
         [("/Type", Name "/Font");
          ("/Subtype", Name "/Type1");
          ("/BaseFont", Name "/Helvetica");])

let symbol_key = "/F0"
let zapf_key = "/F1"
let helvetica_key = "/F2"

type t = {
  x_range : int * int;
  y_range : int * int;
  pixel_size : int;
  fat_line_interval : int;
  page_number : int;
  symbols : Palette.symbol Palette.ColorMap.t;
}

let x_per_page ~pixel_size =
  (* assume 7 usable inches after margins and axis labels *)
  (72 * 7) / pixel_size

let y_per_page ~pixel_size =
  (* assume 9 usable inches after margins, axis labels, and page number *)
  (72 * 9) / pixel_size

let find_upper_left t x y =
  (* on the page, find the right upper-left-hand corner for this pixel
     given the x and y coordinates and `t` representing this page *)
  let left_adjust = x * t.pixel_size
  and top_adjust = y * t.pixel_size in
  (min_x +. (float_of_int left_adjust), max_y -. (float_of_int top_adjust))

let paint_grid_lines t =
  let thickness n = if n mod t.fat_line_interval = 0 then 2. else 1. in
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
    let (left_pt_x, y_pt) = find_upper_left t 0 y in
    let (right_pt_x, _) = find_upper_left t (snd t.x_range - fst t.x_range) y in
    paint_line ~thickness:(thickness (y + fst t.y_range)) (left_pt_x, y_pt) (right_pt_x, y_pt)
  in
  let paint_vertical_line x =
    let (x_pt, top_y_pt) = find_upper_left t x 0 in
    let (_, bottom_y_pt) = find_upper_left t x (snd t.y_range - fst t.y_range) in
    paint_line ~thickness:(thickness (x + fst t.x_range)) (x_pt, top_y_pt) (x_pt, bottom_y_pt)
  in
  let rec paint_horizontal_lines n l =
    let n_lines = (snd t.y_range) - (fst t.y_range) in
    if n > n_lines then l
    else (paint_horizontal_lines (n+1) (paint_horizontal_line n @ l))
  in
  let rec paint_vertical_lines n l =
    let n_lines = (snd t.x_range) - (fst t.x_range) in
    if n > n_lines then l
    else (paint_vertical_lines (n+1) (paint_vertical_line n @ l))
  in
  paint_horizontal_lines 0 [] @ paint_vertical_lines 0 []

let make_grid_label ~text_size n n_x_pos n_y_pos =
  Pdfops.([
      Op_q;
      Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (n_x_pos, n_y_pos)]);
      Op_Tf (helvetica_key, text_size);
      Op_BT; (* begin text object *)
      Op_Tj (string_of_int n); (* actual label *)
      Op_ET; (* done! *)
      Op_Q;
    ])

let label_top_line t n =
  (* label n goes where the nth pixel would go, just nudged up a bit *)
  let (n_x_pos, n_y_pos) = find_upper_left t (n - (fst t.x_range)) 0 in
  make_grid_label ~text_size:(float_of_int t.pixel_size) n n_x_pos (n_y_pos +. (float_of_int t.pixel_size))

let label_left_line t n =
  (* label n goes where the nth pixel would go, just nudged left a bit *)
  (* TODO: it would be good to right-justify this; it's weird-looking with different-width labels *)
  (* is there a way to right-justify this bad boy? *)
  let (n_x_pos, n_y_pos) = find_upper_left t 0 (n - (fst t.y_range)) in
  make_grid_label ~text_size:(float_of_int t.pixel_size) n (n_x_pos -. (float_of_int (t.pixel_size * 2))) n_y_pos

let label_top_grid t =
  let rec label n l =
    if n > snd t.x_range then l
    else
      label (n+t.fat_line_interval) (label_top_line t n @ l) 
  in
  (* find the first grid line that is evenly divisible by fat_line_interval; start there *)
  match (fst t.x_range) mod t.fat_line_interval with
  | 0 -> label (fst t.x_range) []
  | _ -> label (((fst t.x_range / t.fat_line_interval) + 1) * t.fat_line_interval) []

let label_left_grid t =
  let rec label n l =
    if n > snd t.y_range then l
    else
      label (n+t.fat_line_interval) (label_left_line t n @ l) 
  in
  (* find the first grid line that is evenly divisible by fat_line_interval; start there *)
  match (fst t.y_range) mod t.fat_line_interval with
  | 0 -> label (fst t.y_range) []
  | _ -> label (((fst t.y_range / t.fat_line_interval) + 1) * t.fat_line_interval) []

let number_page number =
  Pdfops.([
      Op_q;
      Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate ((72. *. 8.), 36.)]);
      Op_Tf (helvetica_key, 12.);
      Op_BT;
      Op_Tj (string_of_int number);
      Op_ET;
      Op_Q;
    ])

(* note that this paints *only* the pixels - the grid lines should be added later *)
(* (otherwise we end up with a lot of tiny segments when we could have just one through-line,
   and also we can more easily do thicker lines on grid intervals when we put them all in at once) *)
(* x_pos and y_pos are the position of the upper left-hand corner of the pixel on the page *)
let paint_pixel ~pixel_size ~x_pos ~y_pos r g b symbol =
  let stroke_width = 3. in (* TODO this should be relative to the thickness of fat lines *)
  (* the color now needs to come in a bit from the grid sides, since
       we're doing a border rather than a fill; this is the constant
       by which it is inset *)
  let color_inset = (stroke_width *. 0.5) in
  let scale n = (float_of_int n) /. 255.0 in
  let (font_key, symbol) = match symbol with
    | Palette.Zapf symbol -> zapf_key, symbol
    | Symbol symbol -> symbol_key, symbol
  in
  Pdfops.([
      Op_q;
      Op_w stroke_width;
      Op_RG (scale r, scale g, scale b);
      Op_re (x_pos +. color_inset, (y_pos -. pixel_size +. color_inset),
             (pixel_size -. (color_inset *. 2.)),
             (pixel_size -. (color_inset *. 2.)));
      Op_s;
      Op_cm
        (* TODO: find a better way to center this *)
        (Pdftransform.matrix_of_transform
           [Pdftransform.Translate
              ((x_pos +. pixel_size *. 0.3),
               (y_pos -. pixel_size *. 0.6))
           ]);
      Op_Tf (font_key, 12.);
      Op_BT;
      Op_Tj symbol;
      Op_ET;
      Op_Q;
    ])

