open Stitchy.Types

type view = {
  x_off : int;
  y_off : int;
  block_display : [ `Symbol | `Solid ];
  zoom : int;
}

type pane = {
  width : int;
  height : int;
}

(* dimensions of the x-axis labels and y-axis labels
   (the upper-right and lower-left rectangles respectively)
   are implicitly defined by the empty corner and stitch grid
   (the upper-left and lower-right rectangles) *)
type left_pane = {
  empty_corner : pane;
  stitch_grid : pane;
}

let switch_view v =
  match v.block_display with
  | `Symbol -> {v with block_display = `Solid }
  | `Solid -> {v with block_display = `Symbol }

let scroll_right substrate view left_pane =
  let next_page = view.x_off + left_pane.stitch_grid.width
  and last_page = (substrate.max_x - left_pane.stitch_grid.width)
  in
  let best_offset = max 0 @@ min next_page last_page in
  { view with x_off = best_offset }

let scroll_left view left_pane =
  let prev_page = view.x_off - left_pane.stitch_grid.width
  and first_page = 0
  in
  let best_offset = max prev_page first_page in
  { view with x_off = best_offset }

let scroll_down substrate view left_pane =
  let next_page = view.y_off + left_pane.stitch_grid.height
  and last_page = (substrate.max_y - left_pane.stitch_grid.height)
  in
  let best_offset = max 0 @@ min next_page last_page in
  { view with y_off = best_offset }

let scroll_up view left_pane =
  let prev_page = view.y_off - left_pane.stitch_grid.height
  and first_page = 0
  in
  let best_offset = max prev_page first_page in
  { view with x_off = best_offset }

