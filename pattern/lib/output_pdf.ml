type borders = {
  max_x : float;
  max_y : float;
  min_x : float;
  min_y : float;
}

let backstitch_thickness = 1.5
let thick_line_thickness = 1.
let thin_line_thickness = 0.5

let dimensions paper =

  let get_points = Pdfunits.convert 72. Pdfpaper.(unit paper) Pdfunits.PdfPoint in
  let margin_points = Pdfunits.convert 72. Pdfunits.Inch Pdfunits.PdfPoint 0.5 in

  let max_x = (get_points Pdfpaper.(width paper)) -. margin_points
  and max_y = (get_points Pdfpaper.(height paper)) -. margin_points
  and min_x = margin_points
  and min_y = margin_points
  in
  { max_y;
    max_x;
    min_y;
    min_x;
  }


let t1_font name =
  Pdf.(Dictionary
         [("/Type", Name "/Font");
          ("/Subtype", Name "/Type1");
          ("/BaseFont", Name name);])

let symbol_font = t1_font "/Symbol"
and zapf_font = t1_font "/ZapfDingbats"
and helvetica_font = t1_font "/Helvetica"

let symbol_key = "/F0"
and zapf_key = "/F1"
and helvetica_key = "/F2"

type doc = {
  symbols : Stitchy.Symbol.t Stitchy.Types.SymbolMap.t;
  pixel_size : int;
  paper_size : Pdfpaper.t;
  fat_line_interval : int;
}

type page = {
  x_range : int * int;
  y_range : int * int;
  page_number : int;
}


(* TODO both of these should be parameterized by the paper info *)
let x_per_page ~pixel_size =
  (* assume 7 usable inches after margins and axis labels *)
  (72 * 7) / pixel_size

let y_per_page ~pixel_size =
  (* assume 9 usable inches after margins, axis labels, and page number *)
  (72 * 9) / pixel_size

let find_upper_left doc x y =
  let {min_x; max_y; _} = dimensions doc.paper_size in
  (* on the page, find the right upper-left-hand corner for this pixel
     given the x and y coordinates and `t` representing this page *)
  let left_adjust = x * doc.pixel_size
  and top_adjust = y * doc.pixel_size in
  (min_x +. (float_of_int left_adjust), max_y -. (float_of_int top_adjust))

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
    let (left_pt_x, y_pt) = find_upper_left doc 0 y in
    let (right_pt_x, _) = find_upper_left doc (snd page.x_range - fst page.x_range) y in
    paint_line ~thickness:(thickness (y + fst page.y_range)) (left_pt_x, y_pt) (right_pt_x, y_pt)
  in
  let paint_vertical_line x =
    let (x_pt, top_y_pt) = find_upper_left doc x 0 in
    let (_, bottom_y_pt) = find_upper_left doc x (snd page.y_range - fst page.y_range) in
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
      Op_Tf (helvetica_key, text_size);
      Op_BT; (* begin text object *)
      Op_Tj (string_of_int n); (* actual label *)
      Op_ET; (* done! *)
      Op_Q;
    ])

let label_top_line doc page n =
  (* label n goes where the nth pixel would go, just nudged up a bit *)
  let (n_x_pos, n_y_pos) = find_upper_left doc (n - (fst page.x_range)) 0 in
  make_grid_label ~text_size:(float_of_int doc.pixel_size) n n_x_pos (n_y_pos +. (float_of_int doc.pixel_size))

let label_left_line doc page n =
  (* label n goes where the nth pixel would go, just nudged left a bit *)
  (* TODO: it would be good to right-justify this; it's weird-looking with different-width labels *)
  (* is there a way to right-justify this bad boy? *)
  let (n_x_pos, n_y_pos) = find_upper_left doc 0 (n - (fst page.y_range)) in
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

let add_watermark watermark =
  Pdfops.([
      Op_q;
      Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate ((72. *. 0.5), 36.)]);
      Op_Tf (helvetica_key, 12.);
      Op_BT;
      Op_Tj watermark;
      Op_ET;
      Op_Q;
    ])


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

let scale n = (float_of_int n) /. 255.0

(* relative luminance calculation is as described in Web Content
   Accessibility Guidelines (WCAG) 2.0, retrieved from
   https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef *)
let relative_luminance (r, g, b) : float =
  let r_srgb = scale r
  and g_srgb = scale g
  and b_srgb = scale b
  in
  let adjust n = if n <= 0.03928 then n /. 12.92 else Float.pow ((n +. 0.055) /. 1.055) 2.4 in
  0.2126 *. (adjust r_srgb) +. 0.7152 *. (adjust g_srgb) +. 0.0722 *. (adjust b_srgb)

(* to get a contrast ratio, calculate (RL of the lighter color + 0.05) / (RL of the darker color + 0.05). W3C wants that ratio to be at least 4.5:1. *)
let contrast_ratio a b =
  let cr lighter darker = (lighter +. 0.05) /. (darker +. 0.05) in
  let a_rl = relative_luminance a
  and b_rl = relative_luminance b
  in
  if a_rl >= b_rl then cr a_rl b_rl else cr b_rl a_rl

let key_and_symbol s = match s.Stitchy.Symbol.pdf with
  | `Zapf symbol -> zapf_key, symbol
  | `Symbol symbol -> symbol_key, symbol

(* note that this paints *only* the pixels - the grid lines should be added later *)
(* (otherwise we end up with a lot of tiny segments when we could have just one through-line,
   and also we can more easily do thicker lines on grid intervals when we put them all in at once) *)
(* x_pos and y_pos are the position of the upper left-hand corner of the pixel on the page *)
let paint_pixel ~font_size ~pixel_size ~x_pos ~y_pos r g b symbol =
  let stroke_width = 3. in (* TODO this should be relative to the thickness of fat lines *)
  (* the color now needs to come in a bit from the grid sides, since
       we're doing a border rather than a fill; this is the constant
       by which it is inset *)
  let (font_key, symbol) = key_and_symbol symbol in
  let font_stroke, font_paint =
    if contrast_ratio (r, g, b) (255, 255, 255) >= 4.5 then
      Pdfops.Op_RG (scale r, scale g, scale b), Pdfops.Op_rg (scale r, scale g, scale b)
    else
      (* TODO: check the background as well and make sure this won't fade in there *)
      Pdfops.Op_RG (0., 0., 0.), Pdfops.Op_rg (0., 0., 0.)
  in
  let font_location =
    (* y_transform gives us the offset to draw our character in a vertically centered location *)
    let y_transform = Pdfstandard14.baseline_adjustment Pdftext.ZapfDingbats |> float_of_int |> (/.) 1000. in
    (* we can get the text width in millipoints directly *)
    let symbol_width = (Pdfstandard14.textwidth false Pdftext.ImplicitInFontFile Pdftext.ZapfDingbats symbol) |>
                       float_of_int |> (/.) 2000. in
    Pdftransform.Translate
      ((x_pos +. ((pixel_size *. 0.5) -. symbol_width)),
       (y_pos -. pixel_size *. 0.5 -. y_transform))
  in
  Pdfops.([
      Op_q;
      Op_w stroke_width;
      Op_s;
      Op_cm
        (Pdftransform.matrix_of_transform [font_location]);
      font_stroke;
      font_paint;
      Op_Tf (font_key, (float_of_int font_size));
      Op_BT;
      Op_Tj symbol;
      Op_ET;
      Op_Q;
    ])

let symbol_table ~font_size color_to_symbol =
  let paint_symbol description s n =
    let font_key, symbol = key_and_symbol s in
    let vertical_offset = 1. *. 72. in
    let vertical_step n = (font_size + 4) * n |> float_of_int in
    Pdfops.[
      Op_q;
      Op_cm
        (Pdftransform.matrix_of_transform
           [Pdftransform.Translate
              (72., vertical_offset +. vertical_step n);
           ]);
      Op_Tf (font_key, (float_of_int font_size));
      Op_BT;
      Op_Tj symbol;
      Op_Tf (helvetica_key, (float_of_int font_size));
      Op_Tj " : ";
      Op_Tj description;
      Op_ET;
      Op_Q;
    ]
  in
  Stitchy.Types.SymbolMap.fold (fun thread symbol (placement, ops) ->
      let description = Stitchy.DMC.Thread.to_string thread in
      let ops = paint_symbol description symbol placement @ ops in
      (placement + 1, ops)
    ) color_to_symbol (0, [])

let font_resources =
   Pdf.(Dictionary [
       ("/Font", Pdf.Dictionary [(symbol_key, symbol_font);
                                 (zapf_key, zapf_font);
                                 (helvetica_key, helvetica_font)]);
     ])

let symbolpage ~font_size paper symbols =
  let content = [ Pdfops.stream_of_ops @@ snd (symbol_table ~font_size symbols) ] in
  {(Pdfpage.blankpage paper) with content; resources = font_resources;}

(* generate a preview image *)
let coverpage paper ({substrate; layers} : Stitchy.Types.pattern) =
  let {min_x; min_y; max_x; max_y} = dimensions paper in
  let width = float_of_int (substrate.max_x + 1)
  and height = float_of_int (substrate.max_y + 1)
  in
  let px =
    if substrate.max_x < substrate.max_y
    then (max_y -. min_y) /. height
    else (max_x -. min_x) /. width
  in
  let fill_color =
    let (r, g, b) = substrate.background in
    Pdfops.Op_rg (scale r, scale g, scale b)
  in
  (* draw the background image *)
  (* TODO: really need some centering logic here *)
  let background = Pdfops.([
      Op_q;
      Op_w 0.;
      fill_color;
      Op_re (min_x, (max_y -. (px *. height)), (px *. width), (px *. height));
      Op_f ; (* fill path *)
      Op_Q;
    ]) in
  let paint_pixel thread stitch (x, y) =
    let r, g, b = Stitchy.DMC.Thread.to_rgb thread in
    (* x and y have their origin in the upper left, but pdf wants to address from the lower left *)
    (* x coordinates need no transposition, but y do *)
    let pdf_x = min_x +. ((float_of_int x) *. px)
    and pdf_y = max_y -. ((float_of_int (y + 1)) *. px)
    in
    match stitch with
    | Stitchy.Types.Cross _ ->
      Pdfops.([
          Op_q;
          Op_m (0., 0.);
          Op_rg (scale r, scale g, scale b);
          Op_re (pdf_x, pdf_y, px, px);
          Op_f;
          Op_Q;
        ])
    | Stitchy.Types.Back stitch ->
      let ((start_x, start_y), (fin_x, fin_y)) = begin
        match stitch with
        | Top ->
          ((pdf_x, pdf_y +. px),
           (pdf_x +. px, pdf_y +. px))
        | Left ->
          ((pdf_x, pdf_y +. px),
           (pdf_x, pdf_y))
        | Right ->
          ((pdf_x +. px, pdf_y +. px),
           (pdf_x +. px, pdf_y))
        | Bottom ->
          ((pdf_x, pdf_y),
           (pdf_x +. px, pdf_y))
      end
      in
      Pdfops.([
          Op_q;
          Op_RG (scale r, scale g, scale b);
          Op_w backstitch_thickness;
          Op_m (start_x, start_y);
          Op_l (fin_x, fin_y);
          Op_s;
          Op_Q;
        ]);
  in
  let paint_layer (layer : Stitchy.Types.layer) =
    Stitchy.Types.CoordinateSet.fold (fun pixel ops ->
        ops @ paint_pixel layer.thread layer.stitch pixel) layer.stitches []
  in
  let pixels = List.fold_left (fun ops layer -> ops @ paint_layer layer) [] layers in
  let page =
    let open Pdfpage in
    {(blankpage paper) with
     content = [
       Pdfops.stream_of_ops background;
       Pdfops.stream_of_ops pixels;
     ]} in
  page

let make_page doc ~watermark ~first_x ~first_y page_number ~width ~height (pixels : doc -> page -> Pdfops.t list) =
  let xpp = x_per_page ~pixel_size:doc.pixel_size
  and ypp = y_per_page ~pixel_size:doc.pixel_size
  in
  let last_x =
    if width < first_x + xpp then width else first_x + xpp
  and last_y =
    if height < first_y + ypp then height else first_y + ypp
  in
  let page = {
    page_number;
    x_range = (first_x, last_x);
    y_range = (first_y, last_y);
    } in
  {(Pdfpage.blankpage doc.paper_size) with
   Pdfpage.content = [
     Pdfops.stream_of_ops @@ (pixels doc page);
     Pdfops.stream_of_ops @@ paint_grid_lines doc page ;
     Pdfops.stream_of_ops @@ label_top_grid doc page;
     Pdfops.stream_of_ops @@ label_left_grid doc page;
     Pdfops.stream_of_ops @@ add_watermark watermark;
     Pdfops.stream_of_ops @@ number_page page_number;
   ];
   Pdfpage.resources = Pdf.(Dictionary [
       ("/Font", Pdf.Dictionary [(symbol_key, symbol_font);
                                 (zapf_key, zapf_font);
                                 (helvetica_key, helvetica_font)]);
     ])
  }

