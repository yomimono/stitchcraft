open Types

let backstitch_thickness = 3.0
let thick_line_thickness = 1.
let thin_line_thickness = 0.5

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

(* TODO both of these should be parameterized by the paper info *)
let x_per_page ~pixel_size =
  (* assume 7 usable inches after margins and axis labels *)
  (72 * 7) / pixel_size

let y_per_page ~pixel_size =
  (* assume 9 usable inches after margins, axis labels, and page number *)
  (72 * 9) / pixel_size

let get_representation pattern symbols x y =
  let open Stitchy.Types in
  let stitch_repr = function
  | (Stitchy.Types.Cross _, thread) ->
    let color = Stitchy.DMC.Thread.to_rgb thread in
    let symbol = match SymbolMap.find_opt thread symbols with
      | None -> Stitchy.Symbol.default
      | Some symbol -> symbol
    in
    Symbol (color, symbol)
  in
  List.map stitch_repr (Stitchy.Types.stitches_at pattern (x, y))

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
      Op_Tf (helvetica_key, text_size);
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

let key_and_symbol s = match s.Stitchy.Symbol.pdf with
  | `Zapf symbol -> zapf_key, symbol
  | `Symbol symbol -> symbol_key, symbol

(* note that this paints *only* the pixels - the grid lines should be added later *)
(* (otherwise we end up with a lot of tiny segments when we could have just one through-line,
   and also we can more easily do thicker lines on grid intervals when we put them all in at once) *)
(* x_pos and y_pos are the position of the upper left-hand corner of the pixel on the page *)
let paint_pixel ~font_size ~pixel_size ~x_pos ~y_pos r g b symbol =
  let stroke_width = 3. in (* TODO this should be relative to the thickness of fat lines *)
  let (font_key, symbol) = key_and_symbol symbol in
  let font_stroke, font_paint =
    if Colors.contrast_ratio (r, g, b) (255, 255, 255) >= 4.5 then
      Pdfops.Op_RG (Colors.scale r, Colors.scale g, Colors.scale b),
      Pdfops.Op_rg (Colors.scale r, Colors.scale g, Colors.scale b)
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

let paint_backstitch ~x_pos ~y_pos ~pixel_size r g b ((src_x, src_y), (dst_x, dst_y)) =
  (* TODO: handle case where segment crosses 1 or more page boundaries *)
  (* TODO pretty sure this math is 0% right but ok *)
  Pdfops.([
      Op_q;
      Op_RG (Colors.scale r, Colors.scale g, Colors.scale b);
      Op_w backstitch_thickness;
      Op_m (x_pos +. pixel_size *. (float_of_int src_x),
            y_pos +. pixel_size *. (float_of_int src_y));
      Op_l (x_pos +. pixel_size *. (float_of_int dst_x),
            y_pos +. pixel_size *. (float_of_int dst_y));
      Op_s;
      Op_Q;
    ])

let paint_stitch doc page ~font_size pattern (x, y) =
  (* position is based on x and y relative to the range expressed on this page *)
  let open Types in
  let (x_pos, y_pos) = Positioning.find_upper_left doc (x - (fst page.x_range)) (y - (fst page.y_range)) in
  let pixel_size = float_of_int doc.pixel_size in
  let paint_repr = function
    | Symbol ((r, g, b), symbol) ->
      paint_pixel ~font_size ~x_pos ~y_pos
        ~pixel_size r g b symbol
    | Line ((r, g, b), segment) ->
            paint_backstitch ~x_pos ~y_pos ~pixel_size r g b segment
  in
  List.map paint_repr (get_representation pattern doc.symbols x y) |> List.flatten

let paint_stitches ~font_size pattern doc page =
  (* x and y are the relative offsets within the page. *)
  (* (so, even if this is page 3 and the grid that's painted over this will be
     stitches 100-200 (x) and 50-70 (y), x and y will count up from 0 here.
     we will add the correct offset to get the right color when indexing into
     the image.)
     the location math is based on this page-relative counting - so, the pixel
     at the third square down and fourth to the right from the upper-left hand
     corner is at the same location regardless of whether it represents (100, 50) from
     the original image, or (50, 25), or (1024, 768). *)
  let rec aux x y l =
    (* if we've advanced beyond the horizontal edge, start over on the left edge of the next row *)
    if x >= (snd page.Types.x_range) then aux (fst page.Types.x_range) (y+1) l
    (* if we've advanced beyond the vertical edge, time to stop *)
    else if y >= (snd page.y_range) then l
    (* otherwise, let's paint the pixel *)
    else begin
      let this_pixel = paint_stitch doc page ~font_size pattern (x, y) in
      aux (x+1) y (this_pixel @ l)
    end
  in
  aux (fst page.x_range) (fst page.y_range) []

let assign_symbols (layers : Stitchy.Types.layer list )=
  let next = function
    | [] -> (Stitchy.Symbol.default, [])
    | hd::tl -> (hd, tl)
  in
  let threads = List.map (fun (layer : Stitchy.Types.layer) -> layer.Stitchy.Types.thread) layers |> List.sort_uniq Stitchy.DMC.Thread.compare in
  let color_map = Stitchy.Types.SymbolMap.empty in
  List.fold_left (fun (freelist, map) thread ->
      let symbol, freelist = next freelist in
      (freelist, Stitchy.Types.SymbolMap.add thread symbol map))
  (Stitchy.Symbol.printable_symbols, color_map) threads

let make_page doc ~watermark ~first_x ~first_y page_number ~width ~height (pixel_fn : doc -> page -> Pdfops.t list) =
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
     Pdfops.stream_of_ops @@ pixel_fn doc page;
     (* Pdfops.stream_of_ops @@ backstitch_fn doc page; *)
     Pdfops.stream_of_ops @@ paint_grid_lines doc page;
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

let pages ~font_size paper_size watermark ~pixel_size ~fat_line_interval symbols pattern =
  let open Stitchy.Types in
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size
  in
  let width = pattern.substrate.max_x + 1 and height = pattern.substrate.max_y + 1 in
  let doc = { Types.paper_size; pixel_size; fat_line_interval; symbols; } in
  let pixels = paint_stitches ~font_size pattern in
  let rec page x y n l =
    let l = make_page doc ~watermark ~first_x:x ~first_y:y ~width ~height n pixels :: l in
    if (x + xpp) >= width && (y + ypp) >= height then l    
    else if (y + ypp) >= height then page (x+xpp) 0 (n+1) l
    else page x (y + ypp) (n+1) l
  in
  List.rev @@ page 0 0 1 []
