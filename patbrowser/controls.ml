open Stitchy.Types

type filename_entry = Uchar.t list (* reversed *)

type selection = {
  start_cell : (int * int);
  end_cell : (int * int);
}

type mode = 
  | Browse
  | Preview
  | Save of filename_entry

type view = {
  x_off : int;
  y_off : int;
  block_display : [ `Symbol | `Solid ];
}

type state = {
  view : view;
  selection : selection option;
  mode : mode;
}

type pane = {
  width : int;
  height : int;
}

(* selections can happen from any corner to any other corner,
 * but it's usually most convenient to treat them in code
 * as if the start cell was the upper left corner
 * and the end cell is the lower right *)
let normalize_selection {start_cell; end_cell} =
  let start_x, start_y = start_cell
  and end_x, end_y = end_cell
  in
  let min_x = min start_x end_x and max_x = max start_x end_x
  and min_y = min start_y end_y and max_y = max start_y end_y
  in
  {start_cell = (min_x, min_y); end_cell = (max_x, max_y)}


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

let limit_rd_scroll ~last_page proposed =
  max 0 @@ min proposed last_page

let limit_lu_scroll proposed =
  max proposed 0

let page_right substrate view left_pane =
  let next_page = view.x_off + left_pane.stitch_grid.width
  and last_page = (substrate.max_x - left_pane.stitch_grid.width)
  in
  let best_offset = limit_rd_scroll ~last_page next_page in
  { view with x_off = best_offset }

let page_left view left_pane =
  let prev_page = view.x_off - left_pane.stitch_grid.width in
  let best_offset = limit_lu_scroll prev_page in
  { view with x_off = best_offset }

let page_down substrate view left_pane =
  let next_page = view.y_off + left_pane.stitch_grid.height
  and last_page = (substrate.max_y - left_pane.stitch_grid.height)
  in
  let best_offset = limit_rd_scroll ~last_page next_page in
  { view with y_off = best_offset }

let page_up view left_pane =
  let prev_page = view.y_off - left_pane.stitch_grid.height in
  let best_offset = limit_lu_scroll prev_page in
  { view with y_off = best_offset }

let scroll substrate view = function
  | `Right -> { view with x_off = limit_rd_scroll ~last_page:(substrate.max_x - 1) (view.x_off + 1) }
  | `Left -> { view with x_off = limit_lu_scroll (view.x_off - 1) }
  | `Up -> { view with y_off = limit_lu_scroll (view.y_off - 1) }
  | `Down -> { view with y_off = limit_rd_scroll ~last_page:(substrate.max_y - 1) (view.y_off + 1) }

let page substrate view left_pane = function
  | `Right -> page_right substrate view left_pane
  | `Left -> page_left view left_pane
  | `Up -> page_up view left_pane
  | `Down -> page_down substrate view left_pane

let handle_typing state f key =
  match key with
  | `Escape -> (`Quit, state)
  | `Enter -> (`Save f), state
  | `Backspace -> begin
      match f with
      | _::l -> (`Typing l, {state with mode = Save f})
      | _ -> `Typing f, state
    end
  | `Uchar u ->
    let f = u::f in
    `Typing f, {state with mode = Save f}
  | `ASCII a ->
    let f = (Uchar.of_char a)::f in
    `Typing f, {state with mode = Save f}
  | _ -> `Typing f, state
