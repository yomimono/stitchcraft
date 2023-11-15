let on_page page_number n npp =
  if n = 0 then 0
  else if n = (npp * page_number) then 0
  else if (n mod npp) = 0 && (n / npp) > page_number then npp
  else n mod npp

let point_on_page ~page_x ~page_y ~xpp ~ypp (x, y) =
  (on_page page_x x xpp, on_page page_y y ypp)

let float_on_page page_number f npp =
  let n = int_of_float f in
  let int_page = on_page page_number n npp in
  let decimal = f -. (float_of_int n) in
  (float_of_int int_page) +. decimal

let left_to_right ((src_x, src_y), (dst_x, dst_y)) =
  if src_x > dst_x then (dst_x, dst_y), (src_x, src_y)
  else (src_x, src_y), (dst_x, dst_y)

let top_to_bottom ((src_x, src_y), (dst_x, dst_y)) =
  if src_y < dst_y then (src_x, src_y), (dst_x, dst_y)
  else (dst_x, dst_y), (src_x, src_y)

let crosses ~max_n ~src ~dst ~npp =
  (* just make sure they're in the right order *)
  let small = min src dst and large = max src dst in
  if small = large then begin
    (* points are in a line; do they straddle npp? *)
    (* in all cases, if we "straddle" a start or end line,
     * we want to treat that as if we don't cross,
     * so return `No then *)
    if small = 0 then `No
    else if large = (max_n + 1) then `Straddles (max_n + 1)
    else if small mod npp = 0 then `Straddles ((large / npp) * npp)
    else if large mod npp = 0 then `Straddles ((small / npp) * npp)
    else `No
  end
  else if small / npp != large / npp && small mod npp != 0 && large mod npp != 0 then
    `Crosses ((large / npp) * npp)
  else if small / npp != large / npp && large mod npp = 0 then begin
    (* if large / npp is only 1 more than small / npp and large mod npp is 0,
     * then "large" is on small's larger page boundary *)
    (* I don't think there are any circumstances where we care that something
     * *touched* a boundary. For now I'll just say this is 'no' *)
    if (small / npp) = (large / npp) - 1 then `No
    else
      (* the difference is too large - the stitch must cross at least 1 page,
       * which we don't allow. *)
      `Invalid
  end
  else `No

let draw_backstitch ~backstitch_width (r, g, b) ((src_x, src_y), (dst_x, dst_y)) =
  Pdfops.([
      Op_q;
      Op_w backstitch_width;
      Op_RG (r, g, b);
      Op_m (src_x, src_y);
      Op_l (dst_x, dst_y);
      Op_s;
      Op_Q;
    ])

let y_value_at ~x ((src_x, src_y), (dst_x, dst_y)) =
  let smallest, largest = if src_x < dst_x
    then (src_x, src_y), (dst_x, dst_y)
    else (dst_x, dst_y), (src_x, src_y)
  in
  let rise = (snd largest) - (snd smallest)
  and run = (fst largest) - (fst smallest)
  in
  let slope = (float_of_int rise) /. (float_of_int run) in
  let x_distance = x - (fst smallest) in
  (float_of_int x_distance) *. slope +. (float_of_int (snd smallest))

let x_value_at ~y ((src_x, src_y), (dst_x, dst_y)) =
  y_value_at ~x:y ((src_y, src_x), (dst_y, dst_x))

(* which pages does a line from (src) to (dst) cross? *)
let pdfops_of_backstitch ~backstitch_width ~grid_label_size ~margin_size
    ~doc ~max_x ~max_y layer (src, dst) =
  let xpp = Positioning.x_per_page ~grid_label_size ~margin_size
      ~paper:doc.Types.paper_size ~pixel_size:doc.Types.pixel_size
  and ypp = Positioning.y_per_page ~grid_label_size ~margin_size
      ~paper:doc.Types.paper_size ~pixel_size:doc.Types.pixel_size
  in
  let r, g, b = Stitchy.DMC.Thread.to_rgb layer.Stitchy.Types.thread
                |> Colors.ensure_contrast_on_white in
  let (src_x, src_y) = src and (dst_x, dst_y) = dst in
  (* weed out an impossible case: when an entire page is crossed *)
  if abs (dst_x - src_x) > xpp || abs (dst_y - src_y) > ypp then []
  else begin
    let crosses_x = crosses ~max_n:max_x ~src:src_x ~dst:dst_x ~npp:xpp
    and crosses_y = crosses ~max_n:max_y ~src:src_y ~dst:dst_y ~npp:ypp
    in
    let plot = point_on_page ~xpp ~ypp in
    let pdf_plot (x, y) = Positioning.find_upper_left doc x y in
    let draw_on_one_page ~page_x ~page_y =
      let start_point = plot ~page_x ~page_y (src_x, src_y) |> pdf_plot
      and end_point = plot ~page_x ~page_y (dst_x, dst_y) |> pdf_plot
      in
      [(page_x, page_y, draw_backstitch ~backstitch_width (r, g, b) (start_point, end_point))]
    in
    let draw_floats = draw_backstitch ~backstitch_width (r, g, b) in
    match crosses_x, crosses_y with
    | `Invalid, _ | _, `Invalid -> []
    | `Straddles _, `Straddles _ -> [] (* a valid stitch can only straddle one grid line *)
    | `No, `No | `Straddles 0, `No | `No, `Straddles 0 ->
      let page_x = min (src_x / xpp) (dst_x / xpp)
      and page_y = min (src_y / ypp) (dst_y / ypp) in
      draw_on_one_page ~page_x ~page_y
    | `Straddles x_break, `No when x_break = max_x + 1 ->
      let page_x = max_x / xpp (* [sic], we don't want a fresh page just for this *)
      and page_y = src_y / ypp in
      draw_on_one_page ~page_x ~page_y
    | `No, `Straddles y_break when y_break = max_y + 1 ->
      let page_x = src_x / xpp
      and page_y = max_y / ypp in
      draw_on_one_page ~page_x ~page_y
    | `Crosses x_break, `No  ->
      (* sort our points from left to right *)
      let leftmost_point, rightmost_point = left_to_right (src, dst) in

      (* figure out the left and right page x-coordinates in the page grid *)
      let left_page_x = max ((x_break / xpp) - 1) 0 in
      let right_page_x = left_page_x + 1 in
      (* both pages will have the same y-coordinate on the page grid *)
      let page_y = (src_y / ypp) in

      (* simple case to plot: the original points *)
      let pdf_plot (x, y) = Positioning.find_upper_left doc x y in
      let left_page_left = plot ~page_x:left_page_x ~page_y leftmost_point |> pdf_plot
      and right_page_right = plot ~page_x:right_page_x ~page_y rightmost_point |> pdf_plot
      in

      (* tougher to plot: the correct point where the line crosses the page break *)
      let break_y_coordinate = y_value_at ~x:x_break (leftmost_point, rightmost_point) in
      let left_page_break_x = (on_page left_page_x x_break xpp) in
      let right_page_break_x = (on_page right_page_x x_break xpp) in

      (* figure out the pagewise y-coordinate with int math, then tack on our
       * remaining value from the float *)
      let pagewise_float_y = float_on_page page_y break_y_coordinate ypp in

      let left_break = Positioning.plot_float_location doc
          (float_of_int left_page_break_x) pagewise_float_y in
      let right_break = Positioning.plot_float_location doc
          (float_of_int right_page_break_x) pagewise_float_y in
      [(left_page_x, page_y, draw_floats (left_page_left, left_break));
       (right_page_x, page_y, draw_floats (right_break, right_page_right));
      ]
    | `No, `Crosses y_break ->
      let top_point, bottom_point = top_to_bottom (src, dst) in
      let page_x = ((fst top_point) / xpp)
      and top_page_y = max ((y_break / ypp) - 1) 0
      in
      let bottom_page_y = top_page_y + 1 in

      let pdf_plot (x, y) = Positioning.find_upper_left doc x y in
      let top_page_top = plot ~page_x ~page_y:top_page_y top_point |> pdf_plot
      and bottom_page_bottom = plot ~page_x ~page_y:bottom_page_y bottom_point |> pdf_plot
      in

      let top_page_break_y = on_page top_page_y y_break ypp
      and bottom_page_break_y = on_page bottom_page_y y_break ypp in

      let break_x_coordinate = x_value_at ~y:y_break (top_point, bottom_point) in
      let pagewise_float_x = float_on_page page_x break_x_coordinate xpp in

      let top_break = Positioning.plot_float_location doc
          pagewise_float_x (float_of_int top_page_break_y)
      and bottom_break = Positioning.plot_float_location doc
          pagewise_float_x (float_of_int bottom_page_break_y)
      in

      [(page_x, top_page_y, draw_floats (top_page_top, top_break));
       (page_x, bottom_page_y, draw_floats (bottom_break, bottom_page_bottom));
      ]
    | `Straddles x_break, `No ->
      let leftmost_point, rightmost_point = left_to_right (src, dst) in
      let left_page_x = max ((x_break / xpp) - 1) 0
      and page_y = (src_y / ypp)
      in
      let right_page_x = left_page_x + 1 in
      let pdf_plot (x, y) = Positioning.find_upper_left doc x y in

      (* since the line straddles a page break, the src and dst
       * are both plottable on both pages. We just have to ask for both of them. *)
      let left_page_start = plot ~page_x:left_page_x ~page_y leftmost_point |> pdf_plot
      and left_page_end = plot ~page_x:left_page_x ~page_y rightmost_point |> pdf_plot
      and right_page_start = plot ~page_x:right_page_x ~page_y leftmost_point |> pdf_plot
      and right_page_end = plot ~page_x:right_page_x ~page_y rightmost_point |> pdf_plot
      in
      [(left_page_x, page_y, draw_floats (left_page_start, left_page_end));
       (right_page_x, page_y, draw_floats (right_page_start, right_page_end));
      ]
    | `No, `Straddles y_break ->
      let top_point, bottom_point = top_to_bottom (src, dst) in
      let page_x = (src_x / xpp)
      and top_page_y = max ((y_break / ypp) - 1) 0
      in
      let bottom_page_y = top_page_y + 1 in
      let pdf_plot (x, y) = Positioning.find_upper_left doc x y in

      let top_page_start = plot ~page_x ~page_y:top_page_y top_point |> pdf_plot
      and top_page_end = plot ~page_x ~page_y:top_page_y bottom_point |> pdf_plot
      and bottom_page_start = plot ~page_x ~page_y:bottom_page_y top_point |> pdf_plot
      and bottom_page_end = plot ~page_x ~page_y:bottom_page_y bottom_point |> pdf_plot
      in
      [(page_x, top_page_y, draw_floats (top_page_start, top_page_end));
       (page_x, bottom_page_y, draw_floats (bottom_page_start, bottom_page_end));]
    | `Crosses x_break, `Straddles y_break ->
      (* we need to make top and bottom pages for both the left and right sides. *)
      let leftmost_point, rightmost_point = left_to_right (src, dst) in
      let left_page_x = max ((x_break / xpp) - 1) 0 in
      let right_page_x = left_page_x + 1 in
      let top_page_y = max ((y_break / ypp) - 1) 0 in
      let bottom_page_y = top_page_y + 1 in

      let break_point = (x_break, y_break) in
      let left_page_left ~page_y =
        plot ~page_x:left_page_x ~page_y leftmost_point |> pdf_plot
      and left_page_breakpoint ~page_y =
        plot ~page_x:left_page_x ~page_y break_point |> pdf_plot
      and right_page_breakpoint ~page_y =
        plot ~page_x:right_page_x ~page_y break_point |> pdf_plot
      and right_page_right ~page_y =
        plot ~page_x:right_page_x ~page_y rightmost_point |> pdf_plot
      in
      let pages ~page_y =
        [ (left_page_x, page_y, draw_floats (left_page_left ~page_y, left_page_breakpoint ~page_y));
          (right_page_x, page_y, draw_floats (right_page_breakpoint ~page_y, right_page_right ~page_y));
        ]
      in
      pages ~page_y:top_page_y @ pages ~page_y:bottom_page_y
    | `Straddles x_break, `Crosses y_break ->
      let top_point, bottom_point = top_to_bottom (src, dst) in
      let left_page_x = max ((x_break / xpp) - 1) 0 in
      let right_page_x = left_page_x + 1 in
      let top_page_y = max ((y_break / ypp) - 1) 0 in
      let bottom_page_y = top_page_y + 1 in

      let break_point = (x_break, y_break) in
      let top_page_top ~page_x = plot ~page_x ~page_y:top_page_y top_point |> pdf_plot
      and top_page_break ~page_x = plot ~page_x ~page_y:top_page_y break_point |> pdf_plot
      and bottom_page_break ~page_x = plot ~page_x ~page_y:bottom_page_y break_point |> pdf_plot
      and bottom_page_end ~page_x = plot ~page_x ~page_y:bottom_page_y bottom_point |> pdf_plot
      in
      let pages ~page_x =
        [ (page_x, top_page_y, draw_floats (top_page_top ~page_x, top_page_break ~page_x));
          (page_x, bottom_page_y, draw_floats (bottom_page_break ~page_x, bottom_page_end ~page_x))
        ]
      in pages ~page_x:left_page_x @ pages ~page_x:right_page_x
    | `Crosses x_break, `Crosses y_break ->
      let break_point = (x_break, y_break) in
      (* first question: is the break point on our line? *)
      (* if so, this is a special case where we only need to include 2 pages,
       * since all points on the line will be either
       * between the src and breakpoint (one page),
       * or the breakpoint and dst (another page).
       * In other words, it's a nice diagonal. *)
      let y_at_x_break = y_value_at ~x:x_break (src, dst)
      and x_at_y_break = x_value_at ~y:y_break (src, dst)
      in
      let y_diff = Float.abs (y_at_x_break -. (float_of_int y_break)) in
      let x_diff = Float.abs (x_at_y_break -. (float_of_int x_break)) in
      if (y_diff < 0.005 && x_diff < 0.005) then begin
        (* just the 2 pages with those line segments on them *)
        (* somewhat arbitrarily, I've chosen to address them as "top" and "bottom",
         * but we could've just as easily done "left" and "right" *)
        let top_point, bottom_point =
          if src_y < dst_y then ((src_x, src_y), (dst_x, dst_y)) else ((dst_x, dst_y), (src_x, src_y))
        in
        let top_page_x = (fst top_point) / xpp and top_page_y = (snd top_point) / ypp in
        let bottom_page_x = (fst bottom_point) / xpp and bottom_page_y = (snd bottom_point) / ypp in
        let top_page_top = plot ~page_x:top_page_x ~page_y:top_page_y top_point |> pdf_plot
        and top_page_break = plot ~page_x:top_page_x ~page_y:top_page_y break_point |> pdf_plot
        and bottom_page_break = plot ~page_x:bottom_page_x ~page_y:bottom_page_y break_point |> pdf_plot
        and bottom_page_end = plot ~page_x:bottom_page_x ~page_y:bottom_page_y bottom_point |> pdf_plot
        in
        [ (top_page_x, top_page_y, draw_floats (top_page_top, top_page_break));
          (bottom_page_x, bottom_page_y, draw_floats (bottom_page_break, bottom_page_end));
        ]
      end else begin
        (* We need to draw 3 pages of line segments:
         * the start point to whichever break point is closest on the source page,
         * the segment of the line that continues on another page until the next break point,
         * and the segment that finishes off the path on the last page.
         *)
      (* the relative placement of the points will determine which points we connect
       * to the break points. *)
      (* In other words, we have four points - in order to correctly draw a line segment,
       * we need to connect them in the right order. *)
      (* Since we know we cross both the X and Y axis, there are only two options :
       * the points are in the upper left and lower right,
       * or the points are in the upper right and lower left.
       *)
        let leftmost_point, rightmost_point = left_to_right (src, dst)
        and top_point, bottom_point = top_to_bottom (src, dst)
        in
        let draw_intermediate_page ~page_x ~page_y =
            let right_break_near = Positioning.plot_float_location doc
                (float_of_int (on_page page_x x_break xpp))
                (float_on_page page_y y_at_x_break ypp)
            in
            let right_break_far = Positioning.plot_float_location doc
                (float_on_page page_x x_at_y_break xpp)
                (float_of_int (on_page page_y y_break ypp))
            in
            (page_x, page_y, draw_floats (right_break_near, right_break_far))
        in
        (* we cross the y axis first; two pages on the same y-coordinate of the page grid,
         * one on another y-coordinate *)
        if leftmost_point = top_point && x_at_y_break > (float_of_int x_break) then begin
          (* we make these pages: 
           * L | R
           * --+--
           *   | B
           *)
          let left_page_x = (fst leftmost_point) / xpp in
          let right_page_x = left_page_x + 1 in
          let top_page_y = (snd leftmost_point) / ypp in
          let bottom_page_y = top_page_y + 1 in
          (* we need to make three pages:
           * L : left_page_x, first_chunk_y
           * R : right_page_x, first_chunk_y
           * Y : right_page_x, second_chunk_y *)
          (* this goes from the leftmost point to the y at x break *)
          let left_page =
            let left_start = plot ~page_x:left_page_x ~page_y:top_page_y leftmost_point |> pdf_plot
            and left_break = Positioning.plot_float_location doc
                (float_of_int (on_page left_page_x x_break xpp))
                (float_on_page top_page_y y_at_x_break ypp)
            in
            (left_page_x, top_page_y, draw_floats (left_start, left_break))
          in
          (* this goes from the y at x break to the y break *)
          let right_page = draw_intermediate_page ~page_x:right_page_x ~page_y:top_page_y in
          (* this goes from the y at x break to the rightmost point *)
          let b_page =
            let b_enter = Positioning.plot_float_location doc
                (float_on_page right_page_x x_at_y_break xpp)
                (float_of_int (on_page bottom_page_y y_break ypp))
            in
            let right_end = plot ~page_x:right_page_x ~page_y:bottom_page_y rightmost_point |> pdf_plot
            in
            (right_page_x, bottom_page_y, draw_floats (b_enter, right_end))
          in
          [ left_page; right_page; b_page ]
        end else if leftmost_point = top_point then begin
          (* we make these pages:
           * T |
           * --+--
           * B | R
           *)
          let top_page_y = (snd top_point) / ypp in
          let bottom_page_y = top_page_y + 1 in
          let left_page_x = (fst leftmost_point) / xpp in
          let right_page_x = left_page_x + 1 in
          (* T has leftmost_point <-> x_at_y_break, y_break
           * B has x_at_y_break, y_break <-> x_break, y_at_x_break
           * R has x_break, y_at_x_break <-> rightmost_point
           *)
          let t_page =
            let left_start = plot ~page_x:left_page_x ~page_y:top_page_y leftmost_point |> pdf_plot in
            let left_break = Positioning.plot_float_location doc
                (float_on_page left_page_x x_at_y_break xpp)
                (float_of_int (on_page top_page_y y_break ypp))
            in
            (left_page_x, top_page_y, draw_floats (left_start, left_break))
          in
          let b_page = draw_intermediate_page ~page_x:left_page_x ~page_y:bottom_page_y in
          let r_page =
            let r_break = Positioning.plot_float_location doc
                (float_of_int (on_page right_page_x x_break xpp))
                (float_on_page bottom_page_y y_at_x_break ypp)
            in
            let r_end = plot ~page_x:right_page_x ~page_y:bottom_page_y rightmost_point |> pdf_plot in
            (right_page_x, bottom_page_y, draw_floats (r_break, r_end))
          in
          [ t_page; b_page; r_page ]

        end else
          if leftmost_point = bottom_point && x_at_y_break > (float_of_int x_break) then begin
            (* we make these pages: 
           *   | T
           * --+--
           * L | B
           *)
          let top_page_y = (snd top_point) / ypp in
          let bottom_page_y = top_page_y + 1 in
          let left_page_x = (fst leftmost_point) / xpp in
          let right_page_x = left_page_x + 1 in
          (* T has topmost <-> (x_at_y_break, y_break)
           * B has (x_at_y_break, y_break) <-> (x_break, y_at_x_break)
           * L has (x_break, y_at_x_break) <-> bottom
           *)
          let t_page =
            let top_start = plot ~page_x:right_page_x ~page_y:top_page_y top_point |> pdf_plot in
            let top_break = Positioning.plot_float_location doc
                (float_on_page right_page_x x_at_y_break xpp)
                (float_of_int (on_page top_page_y y_break ypp))
            in
            (right_page_x, top_page_y, draw_floats (top_start, top_break))
          in
          let b_page = draw_intermediate_page ~page_x:right_page_x ~page_y:bottom_page_y in
          let l_page =
            let left_break = Positioning.plot_float_location doc
                (float_on_page left_page_x x_at_y_break xpp)
                (float_of_int (on_page bottom_page_y y_break ypp))
            in
            let left_end = plot ~page_x:left_page_x ~page_y:bottom_page_y leftmost_point |> pdf_plot in
            (left_page_x, bottom_page_y, draw_floats (left_break, left_end))
          in
          [t_page; b_page; l_page]
        end else begin
          (* we make these pages:
           * L | R
           * --+--
           * B |
           *)
          let top_page_y = (snd top_point) / ypp in
          let bottom_page_y = top_page_y + 1 in
          let left_page_x = (fst leftmost_point) / xpp in
          let right_page_x = left_page_x + 1 in
          (* B has leftmost <-> x_at_y_break, y_break *)
          (* L has x_at_y_break, y_break <-> x_break, y_at_x_break 
           * R has x_break, y_at_x_break, rightmost *)
          let b_page =
            let b_start = plot ~page_x:left_page_x ~page_y:bottom_page_y leftmost_point |> pdf_plot in
            let b_break = Positioning.plot_float_location doc
                (float_on_page left_page_x x_at_y_break xpp)
                (float_of_int (on_page bottom_page_y y_break ypp))
            in
            (left_page_x, bottom_page_y, draw_floats (b_start, b_break))
          in
          let l_page = draw_intermediate_page ~page_x:left_page_x ~page_y:top_page_y in
          let r_page =
            let r_break = Positioning.plot_float_location doc
                (float_of_int (on_page right_page_x x_break xpp))
                (float_on_page top_page_y y_at_x_break ypp)
            in
            let r_end = plot ~page_x:right_page_x ~page_y:top_page_y rightmost_point |> pdf_plot in
            (right_page_x, top_page_y, draw_floats (r_break, r_end))
          in
          [b_page; l_page; r_page]
      end
      end
  end
