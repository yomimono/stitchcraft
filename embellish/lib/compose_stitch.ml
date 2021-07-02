(* quite a bit of this logic will be similar to the notty concatenation logic *)
open Stitchy.Types

let displace_stitch_down ~amount (x, y) =
  x, (y + amount)

let shift_stitches_down ~amount (layer : layer) =
  {layer with stitches =
                CoordinateSet.map (displace_stitch_down ~amount) layer.stitches}

let displace_stitch_right ~amount (x, y) =
  (x + amount), y

let shift_stitches_right ~amount (layer : layer) =
  {layer with stitches =
                CoordinateSet.map (displace_stitch_right ~amount) layer.stitches}

let shift_layers_down ~amount = List.map (shift_stitches_down ~amount)
let shift_layers_right ~amount = List.map (shift_stitches_right ~amount)

let padding one two =
  let which_to_pad, bigger, smaller =
    if one > two
    then `Second, one, two
    else `First, two, one
  in
  if one = two then `None, 0, 0
  else begin
    let difference = bigger - smaller in
    (* for an even difference, pad evenly on both sides *)
    if difference mod 2 = 0 then (which_to_pad, (difference / 2), (difference / 2))
    (* for an odd difference, favor padding the top or left element *)
    else which_to_pad, ((difference / 2) + 1), (difference / 2)
  end

let vpadding left right = padding left.substrate.max_y right.substrate.max_y
let hpadding top bottom = padding top.substrate.max_x bottom.substrate.max_x

let hcat_with_substrate substrate (upper : pattern) (lower : pattern) =
  let new_max_y = upper.substrate.max_y + lower.substrate.max_y + 1
  and new_max_x = max upper.substrate.max_x lower.substrate.max_x
  in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let which_to_pad, left_padding, _ = hpadding upper lower in
  match which_to_pad with
  | `None ->
    (* displace the lower stitches downward; leave the upper ones as is *)
    let lowered_layers = List.map (shift_stitches_down ~amount:(upper.substrate.max_y + 1) ) lower.layers in
    (* TODO: ugh, backstitch *)
    let layers = Stitchy.Layers.merge_threads upper.layers lowered_layers in
    { substrate; layers; backstitch_layers = []}
  | `First ->
    (* push the lower stitches down by upper.max_y *)
    let lowered_layers = List.map (shift_stitches_down ~amount:(upper.substrate.max_y + 1) ) lower.layers in
    let left_shifted_layers = List.map (shift_stitches_right ~amount:(left_padding)) upper.layers in
    let layers = Stitchy.Layers.merge_threads lowered_layers left_shifted_layers in
    { substrate; layers; backstitch_layers = []}
  | `Second ->
    (* the lower stitches have to be both displaced down and displaced right *)
    let shifted_lower_layers = List.map (fun layer ->
        shift_stitches_down ~amount:(upper.substrate.max_y + 1) layer |>
        shift_stitches_right ~amount:left_padding) lower.layers in
    let layers = Stitchy.Layers.merge_threads shifted_lower_layers upper.layers in
    { substrate; layers; backstitch_layers = []}

(** [hcat upper lower] concatenates two patterns around a horizontal axis.
    If the substrates differ in background color, grid, or block size,
    the upper pattern's substrate will be used. *)
let hcat upper lower =
  hcat_with_substrate upper.substrate upper lower

let vcat_with_substrate substrate left right =
  let new_max_x = left.substrate.max_x + right.substrate.max_x + 1
  and new_max_y = max left.substrate.max_y right.substrate.max_y
  in
  let which_to_pad, top_pad, _ = vpadding left right in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  match which_to_pad with
  | `None ->
    let shifted_layers = List.map (shift_stitches_right ~amount:(left.substrate.max_x + 1)) right.layers in
    {substrate; layers = Stitchy.Layers.merge_threads left.layers shifted_layers;
     backstitch_layers = []}
  | `First ->
    (* shift the left-side elements down to center stuff vertically,
       and also shift the right-side elements right *)
    let downshifted_left_side = List.map (shift_stitches_down ~amount:top_pad) left.layers in
    let rightshifted_right_side = List.map (shift_stitches_right ~amount:(left.substrate.max_x + 1)) right.layers in
    let layers = Stitchy.Layers.merge_threads downshifted_left_side rightshifted_right_side in
    { substrate; layers; backstitch_layers = []}
  | `Second ->
    (* left side stays as is, but right side is both shifted right (for concatenation)
       and shifted down (for vertical centering) *)
    let displaced_right_side =
      List.map (fun layer -> shift_stitches_down ~amount:top_pad layer |>
                             shift_stitches_right ~amount:(left.substrate.max_x + 1))
        right.layers in
    {substrate; layers = Stitchy.Layers.merge_threads left.layers displaced_right_side
    ; backstitch_layers = []}

(** [vcat left right] joins two patterns along a vertical axis.
    If the substrates differ in background color, grid, or block size,
    the left pattern's substrate will be used. *)
let vcat left right =
  vcat_with_substrate left.substrate left right

let rec vrepeat image = function
  | n when n <= 1 -> image
  | n -> vcat image (vrepeat image (n - 1))

let rec hrepeat image = function
  | n when n <= 1 -> image
  | n -> hcat image (hrepeat image (n - 1))

let rotate_ccw (pattern : pattern) =
  let rotate (x, y) =
    let rotated_x = x * -1 in
    let transposed_x = rotated_x + pattern.substrate.max_x in
    (y, transposed_x)
  in
  (* TODO yet again, backstitch *)
  let (layers : layer list) = List.map (fun (layer : layer) ->
      { layer with stitches = CoordinateSet.map rotate layer.stitches }
    ) pattern.layers in
  let substrate = {pattern.substrate with max_x = pattern.substrate.max_y;
                                          max_y = pattern.substrate.max_x} in
  { pattern with layers = layers; substrate = substrate }

let (<->) = hcat
let (<|>) = vcat

let empty base_substrate max_x max_y =
  let substrate = {base_substrate with max_x; max_y } in
  {layers = []; substrate; backstitch_layers = []}

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
