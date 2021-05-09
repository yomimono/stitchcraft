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


let padding one two =
  let which_to_pad, bigger, smaller =
    if one > two
    then `Second, one, two
    else `First, two, one
  in
  if one = two then `None, 0, 0
  else begin
    let difference = bigger - smaller in
    (* for an even difference,pad evenly on both sides *)
    if difference mod 2 = 0 then (which_to_pad, (difference / 2), (difference / 2))
    (* for an odd difference, favor padding the top or left element *)
    else which_to_pad, ((difference / 2) + 1), (difference / 2)
  end

let vpadding left right = padding left.substrate.max_y right.substrate.max_y
let hpadding top bottom = padding top.substrate.max_x bottom.substrate.max_x

let merge_threads layers_a layers_b =
  let is_mergeable a b =
    Stitchy.DMC.Thread.equal a.thread b.thread &&
    Stitchy.Types.equal_stitch a.stitch b.stitch
  in
  let merge (a : layer) (b : layer) =
    {a with stitches = CoordinateSet.union a.stitches b.stitches}
  in
  List.fold_left (fun deduplicated layer ->
      match List.partition (is_mergeable layer) deduplicated with
      | [], deduplicated -> layer :: deduplicated
      | duplicates, sans_duplicates ->
        (* there shouldn't be more than one duplicate here, but it's not hard
           to do the right thing if there is, so let's just go for it *)
        let new_layer = List.fold_left (fun l dupe -> merge l dupe) layer duplicates in
        new_layer :: sans_duplicates
    ) [] (layers_a @ layers_b)

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
    let layers = merge_threads upper.layers lowered_layers in
    { substrate; layers}
  | `First ->
    (* push the lower stitches down by upper.max_y *)
    let lowered_layers = List.map (shift_stitches_down ~amount:(upper.substrate.max_y + 1) ) lower.layers in
    let left_shifted_layers = List.map (shift_stitches_right ~amount:(left_padding)) upper.layers in
    let layers = merge_threads lowered_layers left_shifted_layers in
    { substrate; layers }
  | `Second ->
    (* the lower stitches have to be both displaced down and displaced right *)
    let shifted_lower_layers = List.map (fun layer ->
        shift_stitches_down ~amount:(upper.substrate.max_y + 1) layer |>
        shift_stitches_right ~amount:left_padding) lower.layers in
    let layers = merge_threads shifted_lower_layers upper.layers in
    { substrate; layers}

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
    {substrate; layers = merge_threads left.layers shifted_layers}
  | `First ->
    (* shift the left-side elements down to center stuff vertically,
       and also shift the right-side elements right *)
    let downshifted_left_side = List.map (shift_stitches_down ~amount:top_pad) left.layers in
    let rightshifted_right_side = List.map (shift_stitches_right ~amount:(left.substrate.max_x + 1)) right.layers in
    let layers = merge_threads downshifted_left_side rightshifted_right_side in
    { substrate; layers}
  | `Second ->
    (* left side stays as is, but right side is both shifted right (for concatenation)
       and shifted down (for vertical centering) *)
    let displaced_right_side =
      List.map (fun layer -> shift_stitches_down ~amount:top_pad layer |>
                             shift_stitches_right ~amount:(left.substrate.max_x + 1))
        right.layers in
    {substrate; layers = merge_threads left.layers displaced_right_side}

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

let (<->) = hcat
let (<|>) = vcat

let empty base_substrate max_x max_y =
  let substrate = {base_substrate with max_x; max_y } in
  let layers = [] in
  { substrate; layers}

let embellish ~center ~corner ~top ~side =
  let open Compose in
  let horiz_border_reps = border_repetitions
      ~center:(center.substrate.max_x + 1)
      ~side:(top.substrate.max_x + 1)
  in
  let vert_border_reps = border_repetitions
      ~center:(center.substrate.max_y + 1)
      ~side:(side.substrate.max_y + 1)
  in
  let side_border = hrepeat side vert_border_reps in
  let top_border = vrepeat top horiz_border_reps in
  let center =
    if center.substrate.max_x < top_border.substrate.max_x then begin
      let empty_corner_x = (top_border.substrate.max_x - center.substrate.max_x) / 2 in
      let empty_corner = empty center.substrate (empty_corner_x - 1) 1 in
      (side_border <|> empty_corner <|> center <|> empty_corner <|> side_border)
    end else
      (side_border <|> center <|> side_border)
  in
  (corner <|> top_border <|> corner)
  <->
  center
  <->
  (corner <|> top_border <|> corner)
