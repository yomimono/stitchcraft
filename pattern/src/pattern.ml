(* known issues:
   grid labels can easily be made not to align or display properly with large pixel sizes
   symbol placement is not great
   large images produce inordinately large PDFs (possibly only solvable with region detection/drawing, or another library which knows how to coalesce regions)
   *)

open Cmdliner

let grid_size =
  let doc = "Size of a grid entry representing one stitch, in points. 72 points is one inch." in
  Arg.(value & opt int 10 & info ["pixel-size"; "p"] ~docv:"PIXEL_SIZE" ~doc)

let fat_line_interval =
  let doc = "Interval at which to draw a thicker guideline on the grid and label the axis with intermediate values." in
  Arg.(value & opt int 5 & info ["guideline"; "g"] ~docv:"GUIDELINE" ~doc)

let src =
  let doc = "PNG to put in PDF." in
  Arg.(value & pos 0 file "input.png" & info [] ~docv:"INPUT" ~doc)

let dst =
  let doc = "Filename for output PDF." in
  Arg.(value & opt string "pattern.pdf" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "construct a cross-stitch pattern based on an image" in
  Term.info "pattern" ~doc

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

(* note that this paints *only* the pixels - the grid lines should be added later *)
(* (otherwise we end up with a lot of tiny segments when we could have just one through-line,
   and also we can more easily do thicker lines on grid intervals when we put them all in at once) *)
(* x_pos and y_pos are the position of the upper left-hand corner of the pixel on the page *)
let paint_pixel ~symbols ~pixel_size ~x_pos ~y_pos x y image =
  let scale n = (float_of_int n) /. 255.0 in
  let r, g, b, a, s = Image.read_rgba image x y (fun r g b a ->
      let symbol = match Palette.ColorMap.find_opt (r, g, b) symbols with
        | None -> Palette.Zapf "\x46"
        | Some c -> c
      in
      scale r, scale g, scale b, a, symbol) in
  (* for now, paint anything with *any* amount of opacity *)
  match a with
  | 0 -> []
  | _ ->
    let stroke_width = 3. in (* TODO this should be relative to the thickness of fat lines *)
    (* the color now needs to come in a bit from the grid sides, since
       we're doing a border rather than a fill; this is the constant
       by which it is inset *)
    let color_inset = (stroke_width *. 0.5) in
    let (font_key, s) = match s with
      | Palette.Zapf s -> zapf_key, s
      | Symbol s -> symbol_key, s
    in
    Pdfops.([
        Op_q;
        Op_w stroke_width;
        Op_RG (r, g, b);
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
        Op_Tj s; (* draw the correct symbol *)
        Op_ET;
        Op_Q;
      ])

let find_upper_left t x y =
  (* on the page, find the right upper-left-hand corner for this pixel
     given the x and y coordinates and `t` representing this page *)
  let left_adjust = x * t.pixel_size
  and top_adjust = y * t.pixel_size in
  (min_x +. (float_of_int left_adjust), max_y -. (float_of_int top_adjust))

let paint_pixels t image =
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
    if x >= (snd t.x_range) then
      aux (fst t.x_range) (y+1) l
    else if y >= (snd t.y_range) then
      l
    else begin
      (* position is based on x and y relative to the range expressed on this page *)
      let (x_pos, y_pos) = find_upper_left t (x - (fst t.x_range)) (y - (fst t.y_range)) in
      (* but color is based on the placement in the actual image, so pass those coordinates
         to the painting function *)
      aux (x+1) y (paint_pixel ~symbols:t.symbols ~x_pos ~y_pos ~pixel_size:(float_of_int t.pixel_size)
                     x y image @ l)
    end
  in
  aux (fst t.x_range) (fst t.y_range) []

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

let make_page ~symbols ~first_x ~first_y ~pixel_size ~fat_line_interval page_number image =
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size
  in
  let last_x =
    if image.Image.width < first_x + xpp then image.Image.width else first_x + xpp
  and last_y =
    if image.Image.height < first_y + ypp then image.Image.height else first_y + ypp
  in
  let t = {
    symbols;
    pixel_size;
    fat_line_interval;
    page_number;
    x_range = (first_x, last_x);
    y_range = (first_y, last_y);
  } in
  {(Pdfpage.blankpage Pdfpaper.uslegal) with
   Pdfpage.content = [
     Pdfops.stream_of_ops @@ paint_pixels t image;
     Pdfops.stream_of_ops @@ paint_grid_lines t ;
     Pdfops.stream_of_ops @@ label_top_grid t;
     Pdfops.stream_of_ops @@ label_left_grid t;
     Pdfops.stream_of_ops @@ number_page page_number;
   ];
   Pdfpage.resources = Pdf.(Dictionary [
       ("/Font", Pdf.Dictionary [(symbol_key, symbol_font);
                                 (zapf_key, zapf_font);
                                 (helvetica_key, helvetica_font)]);
     ])
  }

let pages ~pixel_size ~fat_line_interval image =
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size in
  let symbols = Palette.assign_symbols image in
  let rec page x y n l =
    let l = make_page ~symbols ~first_x:x ~first_y:y ~pixel_size ~fat_line_interval n image :: l in
    if (x + xpp) >= image.Image.width && (y + ypp) >= image.Image.height then l    
    else if (y + ypp) >= image.Image.height then page (x+xpp) 0 (n+1) l
    else page x (y + ypp) (n+1) l
  in
  List.rev @@ page 0 0 1 []

let embed_image pixel_size fat_line_interval src dst =
  let image = ImagePNG.ReadPNG.parsefile @@ ImageUtil_unix.chunk_reader_of_path src in
  let pages = pages ~pixel_size ~fat_line_interval image in
  let pdf, pageroot = Pdfpage.add_pagetree pages (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdf in
  Pdfwrite.pdf_to_file pdf dst

let embed_t = Term.(const embed_image $ grid_size $ fat_line_interval $ src $ dst)

let () = Term.exit @@ Term.eval (embed_t, info)
