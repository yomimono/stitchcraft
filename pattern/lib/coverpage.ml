(* generate a preview image *)
let coverpage paper ({substrate; layers; _} : Stitchy.Types.pattern) =
  let Types.{min_x; min_y; max_x; max_y} = Positioning.dimensions paper in
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
    Pdfops.Op_rg (Colors.scale r, Colors.scale g, Colors.scale b)
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
  let paint_coverpage_stitch thread stitch (x, y) =
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
          Op_rg (Colors.scale r, Colors.scale g, Colors.scale b);
          Op_re (pdf_x, pdf_y, px, px);
          Op_f;
          Op_Q;
        ])
  in
  let paint_layer (layer : Stitchy.Types.layer) =
    Stitchy.Types.CoordinateSet.fold (fun pixel ops ->
        ops @ paint_coverpage_stitch layer.thread layer.stitch pixel) layer.stitches []
  in
  let pixels = List.fold_left (fun ops layer -> ops @ paint_layer layer) [] layers in
  let page =
    let open Pdfpage in
    {(blankpage paper) with
     content = [
       Pdfops.stream_of_ops background;
       Pdfops.stream_of_ops pixels;
     ]}
  in
  page
