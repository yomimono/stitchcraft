let orientation substrate paper =
  let open Stitchy.Types in
  (* portrait by default and for images longer than they are wide;
   * landscape for images wider than they are long *)
  if substrate.max_x > substrate.max_y then Pdfpaper.landscape paper else paper

(* generate a preview image *)
let coverpage paper ({substrate; layers; backstitch_layers} : Stitchy.Types.pattern) =
  let paper = orientation substrate paper in
  let backstitch_thickness = 3. in (* TODO this is really arbitrary *)
  let Types.{min_x; min_y; max_x; max_y} = Positioning.dimensions paper in
  let width = float_of_int (substrate.max_x + 1) (* these dimensions are in points *)
  and height = float_of_int (substrate.max_y + 1)
  in
  (* determine how many points each square (or px, for pixel)
   * representing a stitch should be,
   * by looking at the size we have available in each dimension
   * and choosing the smaller of the two. *)
  let smallest_paper_dimension = min (max_x -. min_x) (max_y -. min_y) in
  let largest_paper_dimension = max (max_x -. min_x) (max_y -. min_y) in
  let smallest_substrate_dimension = min width height in
  let largest_substrate_dimension = max width height in
  (* "smallest" and "largest" don't refer to the values in smallest_px and largest_px
   * themselves. Rather, these numbers represent a ratio of points available
   * on the PDF to pixels in the image,
   * so smallest_px could well be a larger value than largest_px, if the image
   * is less square than the paper is. *)
  let smallest_px = smallest_paper_dimension /. smallest_substrate_dimension in
  let largest_px = largest_paper_dimension /. largest_substrate_dimension in
  let px = min smallest_px largest_px in
  let pdf_x x = min_x +. ((float_of_int x) *. px)
  and pdf_y y = max_y -. ((float_of_int (y + 1)) *. px)
  in
  let fill_color =
    let (r, g, b) = substrate.background in
    Pdfops.Op_rg (Colors.scale r, Colors.scale g, Colors.scale b)
  in
  (* before we draw the individual pixels, draw a big ol' rectangle for the background *)
  (* we don't explicitly set Z-levels, but luckily calling the pixel-drawing
   * function after this seems to make sure that this ends up below them *)
  let background = Pdfops.([
      Op_q;
      Op_w 0.;
      fill_color;
      Op_re (min_x, (max_y -. (px *. height)), (px *. width), (px *. height));
      Op_f ; (* fill path *)
      Op_Q;
    ]) in
  let paint_coverpage_stitch thread stitch (x, y) =
    let r, g, b = Stitchy.DMC.Thread.to_rgb thread in
    (* x and y have their origin in the upper left, but pdf wants to address from the lower left *)
    (* x coordinates need no transposition, but y do *)
    match stitch with
    | Stitchy.Types.Cross _ ->
      Pdfops.([
          Op_q;
          Op_m (0., 0.);
          Op_rg (Colors.scale r, Colors.scale g, Colors.scale b);
          Op_re ((pdf_x x), (pdf_y y), px, px);
          Op_f;
          Op_Q;
        ])
  in
  let paint_coverpage_backstitch thread ((src_x, src_y), (dst_x, dst_y)) =
    let r, g, b = Stitchy.DMC.Thread.to_rgb thread in
    Pdfops.([
        Op_q;
        Op_w backstitch_thickness;
        Op_RG (Colors.scale r, Colors.scale g, Colors.scale b);
        (* y coordinates need to be pushed one up because our backstitch grid is the edges
         * of the pixel grid. the x coordinates are fine because the left edge is used
         * both for the base of the rectangles representing the pixels and our count,
         * but in the `y` case we need to move it up one *)
        Op_m (pdf_x src_x, pdf_y (src_y - 1));
        Op_l (pdf_x dst_x, pdf_y (dst_y - 1));
        Op_s;
        Op_Q;
      ]) 
  in
  let paint_layer (layer : Stitchy.Types.layer) =
    Stitchy.Types.CoordinateSet.fold (fun pixel ops ->
        ops @ paint_coverpage_stitch layer.thread layer.stitch pixel) layer.stitches []
  in
  let paint_backstitch_layer (bs_layer : Stitchy.Types.backstitch_layer) =
    Stitchy.Types.SegmentSet.fold (fun segment ops ->
        ops @ paint_coverpage_backstitch bs_layer.thread segment) bs_layer.stitches []
  in
  let pixels = List.fold_left (fun ops layer -> ops @ paint_layer layer) [] layers in
  let backstitches = List.fold_left (fun ops bs_layer -> ops @ paint_backstitch_layer bs_layer) [] backstitch_layers in
  let page =
    let open Pdfpage in
    {(blankpage paper) with
     content = [
       Pdfops.stream_of_ops background;
       Pdfops.stream_of_ops pixels;
       Pdfops.stream_of_ops backstitches;
     ]}
  in
  page
