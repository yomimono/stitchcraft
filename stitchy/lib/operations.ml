open Types

type displacement = | Right of int | Down of int | RightAndDown of int * int

let displace_stitch displacement (x, y) =
  match displacement with
  | Right r -> (x + r, y)
  | Down d -> (x, y + d)
  | RightAndDown (r, d) -> (x + r, y + d)

let expand_substrate displacement s =
  match displacement with
  | Right r -> { s with max_x = s.max_x + r}
  | Down d -> { s with max_y = s.max_y + d}
  | RightAndDown (r, d) -> { s with max_x = s.max_x + r;
                                    max_y = s.max_y + d;}

let displace_backstitch displacement (src, dst) =
  displace_stitch displacement src, displace_stitch displacement dst

let displace_layer d (layer : layer) =
  {layer with stitches = CoordinateSet.map (displace_stitch d) layer.stitches}

let displace_backstitch_layer d (bs : backstitch_layer) =
  {bs with stitches =
             SegmentSet.map (displace_backstitch d) bs.stitches}

let transform_all_stitches ~(f:(int * int) -> (int * int)) pattern =
  let transform_layer (l : layer)  =
    {l with stitches = CoordinateSet.map f l.stitches}
  and transform_bs_layer (bs : backstitch_layer) =
    {bs with stitches = SegmentSet.map (fun (src, dst) -> f src, f dst) bs.stitches}
  in
  {pattern with layers = List.map transform_layer pattern.layers;
               backstitch_layers = List.map transform_bs_layer pattern.backstitch_layers;
  }

let displace_pattern displacement pattern =
  let displaced = transform_all_stitches ~f:(displace_stitch displacement) pattern
  in
  {displaced with substrate = expand_substrate displacement pattern.substrate}

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

let merge_patterns ~substrate a b =
    let layers = Layers.merge_threads a.layers b.layers in
    let backstitch_layers = Layers.merge_backstitch_threads a.backstitch_layers b.backstitch_layers in
    {layers; backstitch_layers; substrate}

let hcat_with_substrate substrate (upper : pattern) (lower : pattern) =
  let new_max_y = upper.substrate.max_y + lower.substrate.max_y + 1
  and new_max_x = max upper.substrate.max_x lower.substrate.max_x
  in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let which_to_pad, left_padding, _ = hpadding upper lower in
  let new_upper, new_lower =
    match which_to_pad with
    | `None ->
      (* shove the lower pattern far enough down to make room for the upper
       * pattern *)
      let displacement = Down (upper.substrate.max_y + 1) in
      (displace_pattern displacement lower, upper)
    | `First ->
      (* the lower stitches go down below the upper ones *)
      (* the upper stitches need to be shifted right to be properly centered in
       * the new pattern, since the lower pattern's width is greater *)
      let lower_displacement = Down (upper.substrate.max_y + 1)
      and upper_displacement = Right left_padding
      in
      (displace_pattern lower_displacement lower,
       displace_pattern upper_displacement upper)
    | `Second ->
      (* the lower stitches have to be both displaced down and displaced right *)
      (* the upper stitches don't need to go anywhere *)
      let displacement = RightAndDown (left_padding, upper.substrate.max_y + 1) in
      (upper, displace_pattern displacement lower)
  in
  (* since the patterns may have layers that match (same thread, same stitch
   * type for non-backstitches) try merging them instead of just appending
   * the lists *)
  merge_patterns ~substrate new_upper new_lower

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
  let (new_left, new_right) =
    match which_to_pad with
    | `None ->
      (* no padding for either pattern, so just shove the right side stuff
       * to the right of the left pattern and merge them *)
      let displacement = Right (left.substrate.max_x + 1) in
      (left, displace_pattern displacement right)
    | `First ->
      (* shift the left-side elements down to center stuff vertically,
         and also shift the right-side elements right *)
      let left_displacement = Down top_pad
      and right_displacement = Right (left.substrate.max_x + 1)
      in
      (displace_pattern left_displacement left,
       displace_pattern right_displacement right)
    | `Second ->
      (* left side stays as is, but right side is both shifted right (for concatenation)
         and shifted down (for vertical centering) *)
      let displacement = RightAndDown (left.substrate.max_x + 1, top_pad) in
      (left, displace_pattern displacement right)
  in
  merge_patterns ~substrate new_left new_right

(** [vcat left right] joins two patterns along a vertical axis.
    If the substrates differ in background color, grid, or block size,
    the left pattern's substrate will be used. *)
let vcat left right =
  vcat_with_substrate left.substrate left right

let hcat_all l =
  match l with
  | [] -> []
  | p :: [] -> p
  | p :: l -> List.fold_left Stitchy.Operations.hcat p l

let vcat_all l =
  match l with
  | [] -> []
  | p :: [] -> p
  | p :: l -> List.fold_left Stitchy.Operations.vcat p l

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
  let rotated = transform_all_stitches ~f:rotate pattern in
  let substrate = {pattern.substrate with max_x = pattern.substrate.max_y;
                                          max_y = pattern.substrate.max_x} in
  { rotated with substrate = substrate }

let (<->) = hcat
let (<|>) = vcat

let empty base_substrate max_x max_y =
  let substrate = {base_substrate with max_x; max_y } in
  {layers = []; substrate; backstitch_layers = []}
