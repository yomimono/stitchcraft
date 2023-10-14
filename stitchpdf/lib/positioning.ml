open Types

let dimensions paper =
  let margin_points = Pdfunits.(points 0.5 Inch) in

  let w = Pdfunits.points (Pdfpaper.width paper) (Pdfpaper.unit paper)
  and h = Pdfunits.points (Pdfpaper.height paper) (Pdfpaper.unit paper)
  in

  let max_x = w -. margin_points
  and max_y = h -. margin_points
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

let plot_float_location doc x y =
  let {min_x; max_y; _} = dimensions doc.paper_size in
  let pixel_size = (float_of_int doc.pixel_size) in
  let left_adjust = x *. pixel_size
  and top_adjust = y *. pixel_size
  in
  (min_x +. left_adjust, max_y -. top_adjust)

let x_per_page ~grid_label_size ~margin_size ~paper ~pixel_size =
  let pixel_size = float_of_int pixel_size in
  let width = Pdfunits.points (Pdfpaper.width paper) (Pdfpaper.unit paper) in
  (* we need to also leave a gutter for the unit labels *)
  let width_in_points = width -. (2. *. margin_size) -. grid_label_size in
  int_of_float @@ width_in_points /. pixel_size

let y_per_page ~grid_label_size ~margin_size ~paper ~pixel_size =
  let pixel_size = float_of_int pixel_size in
  let height = Pdfunits.points (Pdfpaper.height paper) (Pdfpaper.unit paper) in
  let height_in_points = height -. (2. *. margin_size) -. grid_label_size in
  int_of_float @@ height_in_points /. pixel_size
