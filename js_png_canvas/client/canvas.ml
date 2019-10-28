open Stitchy
open Types

let block_size = 10

let thread_to_css thread = Js_of_ocaml.CSS.Color.hex_of_rgb (DMC.Thread.to_rgb thread)

let render_block c (x, y) block =
  let context = c##getContext (Js_of_ocaml.Dom_html._2d_) in
  context##.fillStyle := Js_of_ocaml.Js.string @@ thread_to_css block.thread;
  context##fillRect
    (* x offset *) (float_of_int (x * block_size))
    (* y offset *) (float_of_int (y * block_size))
    (* width *) (float_of_int block_size)
    (* length *) (float_of_int block_size)

let render_grid ?(minor_index= 5) ?(major_index = 20) c piece =
  let linewidth index = match index mod minor_index with
    | 0 -> if index mod major_index = 0 then 3. else 2.
    | _ -> 1.
  in
  let context = c##getContext (Js_of_ocaml.Dom_html._2d_) in
  let draw_col_line index =
    context##beginPath;
    context##.lineWidth := linewidth index;
    let x = float_of_int @@ index * block_size in
    context##moveTo x 0.;
    context##lineTo x
      (float_of_int @@ (piece.max_y + 1) * block_size);
    context##stroke
  in
  let draw_row_line index =
    context##beginPath;
    context##.lineWidth := linewidth index;
    let y = float_of_int @@ index * block_size in
    context##moveTo 0. y;
    context##lineTo (float_of_int @@ (piece.max_x + 1) * block_size)
      y;
    context##stroke
  in
  let rec aux f = function
    | k when k < 0 -> ()
    | n -> (f n; aux f (n-1))
  in
  aux draw_col_line (piece.max_x + 1);
  aux draw_row_line (piece.max_y + 1)

let render_blocks canvas blocks =
  BlockMap.iter (render_block canvas) blocks

(* TODO it can represent a gridline instead, but we'll pretend that isn't true now *)
let which_block _substrate x y =
  (* TODO: this definitely isn't right, substitute this with a real way to find the offset to the first block *)
  (* unfortunately we may need to set it ourselves in order to be able to know what it is *)
  let padding_x, padding_y = 5, 5 in
  ((x - padding_x) / block_size), ((y - padding_y) / block_size)

let render_background canvas substrate =
  (* draw a big ol' rectangle of the background color *)
  let far_x = (substrate.max_x + 1) * block_size
  and far_y = (substrate.max_y + 1) * block_size
  in 
  let context = canvas##getContext (Js_of_ocaml.Dom_html._2d_) in
  context##.fillStyle := Js_of_ocaml.(Js.string @@ CSS.Color.hex_of_rgb substrate.background);
  context##fillRect
    (* x offset *) 0.
    (* y offset *) 0.
    (* width *) (float_of_int far_x)
    (* length *) (float_of_int far_y)

let render canvas state =
  render_background canvas state.substrate;
  render_grid canvas state.substrate;
  render_blocks canvas state.stitches

let paint_pixel state thread stitch mouse_x mouse_y =
  let in_bounds x y =
    x >= 0 && x <= state.substrate.max_x &&
    y >= 0 && y <= state.substrate.max_y
  and (x, y) = which_block state.substrate mouse_x mouse_y in
  match in_bounds x y with
  | false -> state
  | true ->
    (* painting over a previously-populated block is totally OK *)
    let new_stitch = { thread; stitch } in
    { state with stitches = BlockMap.add (x, y) new_stitch state.stitches }

let erase_pixel state mouse_x mouse_y =
  let in_bounds x y =
    x >= 0 && x <= state.substrate.max_x &&
    y >= 0 && y <= state.substrate.max_y
  and (x, y) = which_block state.substrate mouse_x mouse_y in
  match in_bounds x y with
  | false -> state
  | true -> {state with stitches = BlockMap.remove (x, y) state.stitches }

