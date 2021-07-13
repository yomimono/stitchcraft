open Stitchy.Operations

(* TODO: this definitely needs a better name *)
let better_embellish ~corner ~top ~center =
  let horizontal_repetitions =
    let open Stitchy.Types in
    let x_to_fill =
      center.substrate.max_x + 2 * corner.substrate.max_y
      - corner.substrate.max_x
    in
    x_to_fill / top.substrate.max_x +
    (if x_to_fill mod top.substrate.max_x <> 0 then 1 else 0)
  and vertical_repetitions =
    let open Stitchy.Types in
    let y_to_fill =
      center.substrate.max_y + 2 * corner.substrate.max_y
      - corner.substrate.max_x
    in
    (* we still use top.substrate.max_x here, because we'll
     * be rotating the top pattern 90 degrees to use it on the sides *)
    y_to_fill / top.substrate.max_x +
    (if y_to_fill mod top.substrate.max_y <> 0 then 1 else 0)
  in
  let top_border = vrepeat top horizontal_repetitions
  and left_border = hrepeat (rotate_ccw top) vertical_repetitions
  in
  let bottom_border = rotate_ccw @@ rotate_ccw top_border
  and right_border = rotate_ccw @@ rotate_ccw left_border
  in
  (* the straight, repeated borders *)
  let borders = [
    shift_layers_right ~amount:(corner.substrate.max_x + 1) top_border.layers;
    shift_layers_down ~amount:(corner.substrate.max_x + 1) @@
      shift_layers_right ~amount:(corner.substrate.max_x + 1 + top_border.substrate.max_x + 1) right_border.layers;
    shift_layers_right ~amount:(corner.substrate.max_y + 1) @@
      shift_layers_down ~amount:(corner.substrate.max_x + 1 + left_border.substrate.max_y + 1) bottom_border.layers;
    shift_layers_down ~amount:(corner.substrate.max_y + 1) left_border.layers;
  ] |> List.flatten in
  (* the corners *)
  let corners = [
    corner.layers; (* upper left *)
    shift_layers_right ~amount:(corner.substrate.max_x + 1 + top_border.substrate.max_x + 1) (rotate_ccw @@ rotate_ccw @@ rotate_ccw corner).layers; (* upper right *)
    shift_layers_right ~amount:(top_border.substrate.max_x + 1 + corner.substrate.max_y + 1) @@
    shift_layers_down ~amount:(left_border.substrate.max_y + 1 + corner.substrate.max_x + 1) (rotate_ccw @@ rotate_ccw corner).layers; (* lower right *)
    shift_layers_down ~amount:(corner.substrate.max_y + 1 + left_border.substrate.max_y + 1) (rotate_ccw corner).layers; (* lower left *)
  ] |> List.flatten in
  let center_shifted =
    let left_padding = ((top_border.substrate.max_x + corner.substrate.max_y)
                        - center.substrate.max_x) / 2 in
    let top_padding = ((left_border.substrate.max_y + corner.substrate.max_y)
                       - center.substrate.max_y) / 2 in
    shift_layers_down ~amount:(corner.substrate.max_y + 1 + top_padding + 1) @@ 
    shift_layers_right ~amount:(left_border.substrate.max_x + 1 + left_padding + 1) center.layers
  in
  let layers =
    Stitchy.Layers.(merge_threads (center_shifted @
                                   corners @ borders) [])
  in
  let new_substrate = { center.Stitchy.Types.substrate with
                        max_x = corner.substrate.max_x + 1 +
                                top_border.substrate.max_x + 1 +
                                corner.substrate.max_y + 1;
                        max_y = left_border.substrate.max_y + 1 +
                                corner.substrate.max_x + 1 +
                                corner.substrate.max_y + 1;
                      } in
  { substrate = new_substrate; layers; backstitch_layers = [] }

let embellish ~rotate_corners ~center ~corner ~top ~side =
  let open Compose in
  let horiz_border_reps = border_repetitions
      ~center:(center.substrate.max_x + 1)
      ~side:(top.substrate.max_x + 1)
  in
  let vert_border_reps = border_repetitions
      ~center:(center.substrate.max_y + 1)
      ~side:(side.substrate.max_y + 1)
  in
  let divide_space amount =
    if amount mod 2 == 0 then (amount / 2, amount / 2)
    else (amount / 2 + 1, amount / 2)
  in
  let side_border = hrepeat side vert_border_reps in
  let top_border = vrepeat top horiz_border_reps in
  (* upper-left, upper-right, lower-right, lower-left *)
  let ul, ur, lr, ll =
    match rotate_corners with
    | false -> corner, corner, corner, corner
    | true -> corner, (rotate_ccw @@ rotate_ccw @@ rotate_ccw corner),
              rotate_ccw @@ rotate_ccw corner, rotate_ccw corner
  in
  let center =
    if center.substrate.max_x < top_border.substrate.max_x then begin
      let x_difference = top_border.substrate.max_x - center.substrate.max_x in
      let left_pad, right_pad = divide_space x_difference in
      let empty_corner_left = empty center.substrate (left_pad - 1) 1 in
      let empty_corner_right = empty center.substrate (right_pad - 1) 1 in
      (side_border <|> empty_corner_left <|> center <|> empty_corner_right <|> side_border)
    end else
      (side_border <|> center <|> side_border)
  in
  (ul <|> top_border <|> ur)
  <->
  center
  <->
  (ll <|> top_border <|> lr)
