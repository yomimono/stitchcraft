open Types

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
let x_per_page ~paper:_ ~pixel_size =
  (* assume 7 usable inches after margins and axis labels *)
  (72 * 7) / pixel_size

let y_per_page ~paper:_ ~pixel_size =
  (* assume 9 usable inches after margins, axis labels, and page number *)
  (72 * 9) / pixel_size

let symbol_of_color symbols thread =
  match Stitchy.Types.SymbolMap.find_opt thread symbols with
    | None -> Stitchy.Symbol.default
    | Some symbol -> symbol

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
  let paint_symbol (r, g, b) description s n =
    let font_key, symbol = key_and_symbol s in
    let r, g, b = Colors.scale r, Colors.scale g, Colors.scale b in
    let vertical_offset = 1. *. 72. in
    let vertical_step n = (font_size + 4) * n |> float_of_int in
    let swatch_x_offset = 72. -. vertical_step 1 in
    Pdfops.[
      Op_q;
      Op_rg (r, g, b);
      Op_re (swatch_x_offset, vertical_offset +. vertical_step n, vertical_step 1, vertical_step 1);
      Op_f;
      Op_Q;
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
      let color = Stitchy.DMC.Thread.to_rgb thread in
      let ops = paint_symbol color description symbol placement @ ops in
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

let page_of_stitch ~pixel_size ~paper (x, y) =
  let xpp = x_per_page ~paper ~pixel_size
  and ypp = y_per_page ~paper ~pixel_size
  in
  let page_row = x / xpp
  and page_column = y / ypp
  and pagewise_x = x mod xpp
  and pagewise_y = y mod ypp
  in
  (page_row, page_column, pagewise_x, pagewise_y)

let pdfops_of_stitch ~font_size ~doc ~(layer : Stitchy.Types.layer) (x, y) =
  let open Stitchy.Types in
  let page_row, page_column, pagewise_x, pagewise_y = page_of_stitch ~pixel_size:doc.pixel_size ~paper:doc.paper_size (x, y) in
  let r, g, b = Stitchy.DMC.Thread.to_rgb layer.thread in
  (* x_pos and y_pos are the PDF-oriented coordinates of the upper left pixel on the
   * specific page, expressed as points *)
  let x_pos, y_pos = Positioning.find_upper_left doc pagewise_x pagewise_y in
  let symbol = symbol_of_color doc.symbols layer.thread in
  let pdfops = paint_pixel ~font_size ~pixel_size:(float_of_int doc.pixel_size)
      ~x_pos ~y_pos r g b symbol in
  (page_row, page_column, pdfops)

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

module PageMap = Map.Make(Stitchy.Types.Coordinates)

let add_pdfops_to_pagemap map (page_row, page_column, pdfops) =
  match PageMap.find_opt (page_row, page_column) map with
  | None -> PageMap.add (page_row, page_column) (pdfops::[]) map
  | Some l -> PageMap.add (page_row, page_column) (pdfops::l) map

let add_layer_to_pagemap map ~doc ~font_size (layer : Stitchy.Types.layer) =
  let open Stitchy.Types in
  CoordinateSet.fold (fun (x, y) acc ->
      let page_x, page_y, pdfops = pdfops_of_stitch ~doc ~font_size ~layer (x, y) in
      add_pdfops_to_pagemap acc (page_x, page_y, pdfops)
    ) layer.stitches map

let populate_pagemap ~doc ~font_size pattern =
  let map = PageMap.empty in
  List.fold_left (fun map layer -> add_layer_to_pagemap map ~doc ~font_size layer)
    map pattern.Stitchy.Types.layers

(* page_x and page_y are the pages' position in a supergrid.
 * i.e. if you were to lay out all the pages of the chart, page (0, 0) should go
 * at the upper left, (0, 1) immediately to its right, (1, 0) immediately below it. *)
let pdfpage_of_page ~substrate ~page_number ~doc ~watermark (page_x, page_y) pdfops =
  let xpp = x_per_page ~paper:doc.paper_size ~pixel_size:doc.pixel_size
  and ypp = y_per_page ~paper:doc.paper_size ~pixel_size:doc.pixel_size
  in
  let first_x page_x = page_x * xpp
  and first_y page_y = page_y * ypp
  in
  let last_x page_x = min (substrate.Stitchy.Types.max_x + 1) @@ first_x (page_x + 1) in
  let last_y page_y = min (substrate.Stitchy.Types.max_y + 1) @@ first_y (page_y + 1) in
  let page = {
    page_number;
    x_range = first_x page_x, last_x page_x;
    y_range = first_y page_y, last_y page_y;
    } in
  {(Pdfpage.blankpage doc.paper_size) with
   Pdfpage.content = [
     Pdfops.stream_of_ops pdfops;
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
  let xpp = x_per_page ~paper:paper_size ~pixel_size
  and ypp = y_per_page ~paper:paper_size ~pixel_size
  in
  let max_page_x = pattern.substrate.max_x / xpp in
  let max_page_y = pattern.substrate.max_y / ypp in
  let doc = { Types.paper_size; pixel_size; fat_line_interval; symbols; } in
  let pagemap = populate_pagemap ~doc ~font_size pattern in
  (* if there are any pages that didn't have any stitches on them, they won't be represented in the page map.
   * but we want to make an empty grid for those, even though it doesn't convey
   * much information -- it's really confusing when a page is just "missing" from
   * your pattern, even if the page numbers are contiguous. *)
  let rec page x y n l =
    if x > max_page_x && y >= max_page_y then List.rev l
    else if x > max_page_x then page 0 (y+1) n l
    else begin
      let p =
        let pdfops = 
          match PageMap.find_opt (x, y) pagemap with
          | None -> []
          | Some p -> List.flatten p
        in
        pdfpage_of_page ~substrate:pattern.substrate ~page_number:n ~doc ~watermark (x, y) pdfops
      in
      page (x + 1) y (n + 1) (p::l)
    end
  in
  page 0 0 1 []
