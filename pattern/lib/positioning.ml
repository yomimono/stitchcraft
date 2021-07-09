open Types

let dimensions paper =
  let dpi = 72. in
  let get_points = Pdfunits.convert dpi Pdfpaper.(unit paper) Pdfunits.PdfPoint in
  let margin_points = Pdfunits.(convert dpi Inch PdfPoint 0.5) in

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

let find_upper_left doc x y =
  let {min_x; max_y; _} = dimensions doc.paper_size in
  (* on the page, find the right upper-left-hand corner for this pixel
     given the x and y coordinates and `t` representing this page *)
  let left_adjust = x * doc.pixel_size
  and top_adjust = y * doc.pixel_size in
  (min_x +. (float_of_int left_adjust), max_y -. (float_of_int top_adjust))

