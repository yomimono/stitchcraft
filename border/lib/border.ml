open Stitchy.Operations

type dimensions = {
  x_off : int;
  y_off : int;
  width : int;
  height : int;
}

let pp fmt {x_off; y_off; width; height} =
  Format.fprintf fmt "%d x %d starting at %d , %d" width height x_off y_off

let within ~x ~y {x_off; y_off; width; height} =
  x_off <= x && x < (x_off + width) &&
  y_off <= y && y < (y_off + height)

let max3 a b c : int = max (max a b) (max b c)

let border_repetitions ~fencepost ~center ~side =
  match center mod (side + fencepost) with
  (* since we need to insert another fencepost anyway, leftovers <= the size of the last fencepost are nothing to worry about *)
  | w when w <= fencepost -> center / (side + fencepost)
  | _ -> (* too much space left over; add another repetition *)
    center / (side + fencepost) + 1

let backstitch_in (src, dst) dim =
  (dim.x_off <= (fst src) && (fst src) <= dim.x_off + dim.width &&
   dim.y_off <= (snd src) && (snd src) <= dim.y_off + dim.height) &&
  (dim.x_off <= (fst dst) && (fst dst) <= dim.x_off + dim.width &&
   dim.y_off <= (snd dst) && (snd dst) <= dim.y_off + dim.height)

(** [tile pattern ~dimensions ~mask_dimensions] fills an area of `dimensions` size with
 * tiling repetitions of `pattern`, except for any dimensions in `mask_dimensions`,
 * which are left unfilled but do not interrupt the tiling. *)
let tile pattern ~(dimensions : dimensions) ~mask_dimensions =
  let open Stitchy.Types in
  let row pattern ~(dimensions : dimensions) =
    let vrepetitions =
      if dimensions.width mod (pattern.substrate.max_x + 1) = 0 then
        dimensions.width / (pattern.substrate.max_x + 1)
      else dimensions.width / (pattern.substrate.max_x + 1) + 1
    in
    let r = Stitchy.Operations.vrepeat pattern vrepetitions in
    {r with substrate = {r.substrate with max_x = (dimensions.width - 1)}}
  in
  let hrepetitions =
    if dimensions.height mod (pattern.substrate.max_y + 1) = 0 then
      dimensions.height / (pattern.substrate.max_y + 1)
    else dimensions.height / (pattern.substrate.max_y + 1) + 1
  in
  let r = Stitchy.Operations.hrepeat (row pattern ~dimensions) hrepetitions in
  let unmasked = {r with substrate = {r.substrate with max_y = dimensions.height - 1}} in
  let shifted = displace_pattern (RightAndDown (dimensions.x_off, dimensions.y_off)) unmasked in
  let stitch_masks : CoordinateSet.t =
    List.fold_left
      (fun so_far (mask : dimensions) ->
         let xs = List.init mask.width (fun n -> n + mask.x_off) in
         let ys = List.init mask.height (fun n -> n + mask.y_off) in
         List.fold_left (fun cs x ->
             List.fold_left (fun cs y ->
                 CoordinateSet.add (x, y) cs
               ) cs ys)
           so_far xs) CoordinateSet.empty mask_dimensions
  in
  let max_x = dimensions.width + dimensions.x_off - 1
  and max_y = dimensions.height + dimensions.y_off - 1
  in
  let mask_layer (layer : layer) =
    let stitches = CoordinateSet.(diff layer.stitches stitch_masks |>
                                  filter (fun (x, y) -> x <= max_x && y <= max_y))
    in
    {layer with stitches;}
  in
  let mask_backstitch_layer (backstitch_layer : backstitch_layer) =
    let not_in_any segment dimensions =
      not @@ List.exists (backstitch_in segment) dimensions
    in
    let backstitches = SegmentSet.filter
        (fun segment -> (not_in_any segment mask_dimensions) &&
                        backstitch_in segment dimensions)
        backstitch_layer.stitches in
    {backstitch_layer with stitches = backstitches}
  in
  {shifted with layers = List.map mask_layer shifted.layers;
                backstitch_layers = List.map mask_backstitch_layer shifted.backstitch_layers;
  }

(* TODO: this definitely needs a better name. It's been renamed a couple times but this TODO remains true *)
(* this is the guilloche-style corner-plus repeating border *)
let wide_corner_embellish ~fill ~corner ~top ~center ~min_width ~min_height =
  let corner_long_side = corner.Stitchy.Types.substrate.max_x + 1 in
  let corner_short_side = corner.substrate.max_y + 1 in
  let horizontal_repetitions =
    let open Stitchy.Types in
    (* if center is smaller than the corners already cover,
     * we need 0 repetitions *)
    (* a rectangular corner will cover (long_side - short_side) pixels of the border *)
    let x_to_fill = max 0 @@
      (max min_width @@ center.substrate.max_x + 1) - (corner_long_side - corner_short_side)
    in
    if x_to_fill > 0 then
      x_to_fill / (top.substrate.max_x + 1) +
      (* if necessary, add an extra repetition *)
      (if x_to_fill mod (top.substrate.max_x + 1) <> 0 then 1 else 0)
    else 0
  and vertical_repetitions =
    let open Stitchy.Types in
    let y_to_fill = max 0 @@
      (max min_height @@ (center.substrate.max_y + 1)) - (corner_long_side - corner_short_side)
    in
    (* we still use top.substrate.max_x here, because we'll
     * be rotating the top pattern 90 degrees to use it on the sides *)
    if y_to_fill > 0 then
      y_to_fill / (top.substrate.max_x + 1) +
      (if y_to_fill mod (top.substrate.max_x + 1) <> 0 then 1 else 0)
    else 0
  in
  (* the straight, repeated borders *)
  let top_border_width = horizontal_repetitions * (top.substrate.max_x + 1) in
  let left_border_length = vertical_repetitions * (top.substrate.max_x + 1) in
  let borders =
    match horizontal_repetitions, vertical_repetitions with
    | 0, 0 -> []
    | n, 0 -> (* we only need more stuff on the top and bottom *)
      let top_border = vrepeat top n in
      let bottom_border = rotate_ccw @@ rotate_ccw top_border in
      [
        displace_pattern (Right corner_long_side) top_border;
        (* bottom border needs to move to the right to make room for the lower left-
         * hand corner (on the short side), and to go to the bottom of the pattern *)
        displace_pattern (RightAndDown (corner_short_side,
                                        corner_long_side))
          bottom_border;
      ]
    | 0, n -> (* we only need more stuff on the left and right *)
      let left_border = hrepeat (rotate_ccw top) n in
      let right_border = rotate_ccw @@ rotate_ccw left_border in
      [
        (* right-hand border needs to move to the right side,
         * and also to move down to make room for the upper right-hand corner *)
        displace_pattern (RightAndDown (corner_long_side,
                                        corner_short_side))
          right_border;
        (* the left border is already on the left side, and just needs to shift down
         * to make room for the upper left-hand corner's short side *)
        displace_pattern (Down corner_short_side) left_border;
      ]
    | horizontal, vertical -> (* we need h more top/bottom, v more left/right *)
      let top_border = vrepeat top horizontal in
      let left_border = hrepeat (rotate_ccw top) vertical in
      let bottom_border = rotate_ccw @@ rotate_ccw top_border in
      let right_border = rotate_ccw @@ rotate_ccw left_border in
      [displace_pattern (Right corner_long_side) top_border;
       displace_pattern (RightAndDown (corner_long_side + top_border_width,
                                       corner_long_side)) right_border;
       displace_pattern (RightAndDown (corner_short_side,
                                       left_border_length + corner_long_side)) bottom_border;
       displace_pattern (Down corner_short_side) left_border;

      ]
  in
  (* the corners *)
  let corners = [
    (* upper left is just what we were passed *)
    corner;
    (* upper right is upper left rotated clockwise 90 degrees,
  r  * then shoved over to the right edge *)
    rotate_ccw @@ rotate_ccw @@ rotate_ccw corner |>
    displace_pattern (Right (corner_long_side + top_border_width));
    (* lower right is upper left rotated 180 degrees,
     * then displaced to the right of the bottom border
     * and below the right-side border *)
    rotate_ccw @@ rotate_ccw corner |>
    displace_pattern (RightAndDown (top_border_width + corner_short_side,
                                    corner_long_side + left_border_length));
    (* lower left is rotated counter-clockwise 90 degrees,
     * then shifted down past the short end of the upper-left corner and the
     * whole left-side border. *)
    rotate_ccw corner |>
    displace_pattern (Down (corner_short_side + left_border_length));
  ] in
  let substrate = { center.Stitchy.Types.substrate with
                    max_x = corner_long_side + corner_short_side +
                            top_border_width - 1;
                    max_y = corner_long_side + corner_short_side +
                            left_border_length - 1
                  } in
  let left_padding, top_padding = 
    (* the center will be smaller than the borders,
     * so not only does it need to move down and to the right
     * to avoid the borders, it also needs to (potentially)
     * move down and to the right to center itself within them. *)
    (* unfortunately, monospaced fonts tend to bias toward having their spacing on the right,
     * so if we also have our bias there we get some weird effects when centering.
     * Instead, prefer to move stuff to the left if there is an uneven number of pixels. *)
    let horizontal_padding = substrate.max_x - center.substrate.max_x
    and vertical_padding = substrate.max_y - center.substrate.max_y
    in
    (max 0 @@ horizontal_padding / 2 + (horizontal_padding mod 2)),
    (max 0 @@ vertical_padding / 2 + (horizontal_padding mod 2))
  in
  let center_shifted = displace_pattern (RightAndDown (left_padding, top_padding)) center in
  match left_padding, top_padding with
  | 0, 0 -> (* nothing to fill! *)
    List.fold_left (merge_patterns ~substrate) center_shifted (corners @ borders)
  | _, _ ->
    let mask_off_center : dimensions =
      {x_off = left_padding;
       y_off = top_padding;
       width = center.substrate.max_x + 1;
       height = center.substrate.max_y + 1 }
    in
    let (potential_fill : dimensions) = {
      x_off = corner_short_side;
      y_off = corner_short_side;
      width = (substrate.max_x + 1) - 2 * corner_short_side;
      height = (substrate.max_y + 1) - 2 * corner_short_side;
    } in
    let center_fill = tile fill ~dimensions:potential_fill ~mask_dimensions:[mask_off_center] in
    List.fold_left (merge_patterns ~substrate) center_shifted ( center_fill :: corners @ borders)

(* this is the simpler, corners-and-repeating-motif kind of repeating border *)
(* TODO there's no reason square_corner_embellish shouldn't have a fill option *)
let square_corner_embellish ~min_width ~rotate_corners ~center ~corner ~top ~fencepost =
  let open Stitchy.Types in
  let open Stitchy.Operations in
  let side = rotate_ccw top in
  let fencepost_w = match fencepost with
    | None -> 0
    | Some fencepost -> fencepost.substrate.max_x + 1
  in
  let center_width = max min_width (center.substrate.max_x + 1) in
  let horiz_border_reps = border_repetitions ~center:center_width
      ~fencepost:fencepost_w
      ~side:(top.substrate.max_x + 1)
  in
  let vert_border_reps = border_repetitions
      ~fencepost:fencepost_w (* sic. we use fencepost_w here again because fencepost gets rotated *)
      ~center:(center.substrate.max_y + 1)
      ~side:(side.substrate.max_y + 1) (* side is already rotated, so use its max_y *)
  in
  let divide_space amount =
    if amount mod 2 == 0 then (amount / 2, amount / 2)
    else (amount / 2 + 1, amount / 2)
  in
  let side_border = match fencepost with
    | None -> hrepeat side vert_border_reps
    | Some fencepost ->
      hcat (hrepeat (hcat (rotate_ccw fencepost) side) vert_border_reps) (rotate_ccw fencepost)
  in
  let top_border = match fencepost with
    | None -> vrepeat top horiz_border_reps
    | Some fencepost ->
      vcat (vrepeat (vcat fencepost top) horiz_border_reps) fencepost
  in
  let left, right = match rotate_corners with
    | false -> side_border, side_border
    | true -> side_border, rotate_ccw @@ rotate_ccw side_border
  in
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
      (left <|> empty_corner_left <|> center <|> empty_corner_right <|> right)
    end else
      (left <|> center <|> right)
  in
  (ul <|> top_border <|> ur)
  <->
  center
  <->
  (ll <|> rotate_ccw @@ rotate_ccw top_border <|> lr)

let width p = p.Stitchy.Types.substrate.max_x + 1
let height p = p.Stitchy.Types.substrate.max_y + 1

let halfway_with_left_bias ~whole ~part =
  let gap = whole - part in
  (* for a gap of uneven size, make the first element larger *)
  let left_part = ((gap / 2) + (gap mod 2)) in
  left_part, (gap / 2)

let assemble_rotated_borders ~left ~right ~top ~bottom ~center =
  let open Stitchy.Types in
  let final_width = (width top) + (width right) in
  let final_height = (height left) + (height top) in
  let full_substrate = { center.substrate with max_x = final_width - 1;
                                               max_y = final_height - 1;
                       } in
  let background = { substrate = full_substrate;
                     layers = [];
                     backstitch_layers = []
                   } in
  let place = merge_patterns ~substrate:full_substrate in
  let right_displacement = Right (width top) in
  let left_displacement = Down (height top) in
  let bottom_displacement = RightAndDown (width left, height right) in
  let center_displacement = RightAndDown (width left, height top) in
  let with_top = place background top in
  let with_left = place with_top (displace_pattern left_displacement left) in
  let with_right = place with_left (displace_pattern right_displacement right) in
  let with_bottom = place with_right (displace_pattern bottom_displacement bottom) in
  let with_center = place with_bottom (displace_pattern center_displacement center) in
  with_center

let expand_center ~desired_width ~desired_height ~pattern ~fill =
  let fill = match fill with
    | Some fill -> fill
    | None -> (* 1x1 empty pattern, of the same substrate as pattern *)
      let open Stitchy.Types in
      let substrate = { pattern.substrate with max_x = 1; max_y = 1} in
      { substrate; layers = []; backstitch_layers = [] }
  in
  (* tile-fill the center including the padding, masking off the original center dimensions. *)
  let dimensions : dimensions = { x_off = 0; y_off = 0;
                                  width = desired_width;
                                  height = desired_height; } in
  let x_off, y_off =
    fst @@ halfway_with_left_bias ~whole:desired_width ~part:(width pattern),
    fst @@ halfway_with_left_bias ~whole:desired_height ~part:(height pattern)
  in
  let mask_dimensions = [{
      width = (width pattern);
      height = (height pattern);
      x_off;
      y_off;
    }] in
  let background = tile fill ~dimensions ~mask_dimensions in
  (* now drop the pattern in the masked-off area *)
  let displacement = RightAndDown (x_off, y_off) in
  let shifted_pattern = displace_pattern displacement pattern in
  merge_patterns ~substrate:(background.substrate) background shifted_pattern

let just_corner ~(center : Stitchy.Types.pattern) ~corner ~fill =
  let open Stitchy.Types in
  let width p = p.substrate.max_x + 1 in
  let height p = p.substrate.max_y + 1 in
  match corner.transformation with
  | Nothing -> begin
      (* easiest case I guess? *)
    let width_needed = border_repetitions ~fencepost:0 ~side:(width corner.pattern) ~center:(width center) in
    let height_needed = border_repetitions ~fencepost:0 ~side:(height corner.pattern) ~center:(height center) in
    let fill_width = (width_needed * (width corner.pattern)) in
    let fill_height = (height_needed * (height corner.pattern)) in
    (* top and bottom are the same, as are left and right, so just use top/left for both *)
    let top = vrepeat corner.pattern (width_needed + 2) in
    let left = hrepeat corner.pattern height_needed in
    let tiled_center = expand_center ~desired_width:fill_width ~desired_height:fill_height ~pattern:center ~fill in
      top
        <->
      (left <|> tiled_center <|> left)
        <->
        top
  end
  | Turn ->
    (* since the corner image will be rotated, we have to consider its different
     * dimensions in our calculations. The additional space on the x-axis isn't
     * just the corner's x-axis times 2, but rather the corner's x-axis plus
     * the corner's y-axis, since the corner pattern will appear along each side
     * at least once in the original orientation and at least once rotated 90
     * degrees from that. *)
    (* the number of top repetitions is at least the number required to clear
     * the size of the long side (for the corner case, no pun intended,
     * where the corner case is taller than it is wide),
     * plus the width of the center image. *)
    let width_needed = border_repetitions ~fencepost:0 ~center:((height corner.pattern) + (width center)) ~side:(width corner.pattern) in
    let height_needed = border_repetitions ~fencepost:0 ~center:((height corner.pattern) + (height center)) ~side:(width corner.pattern) in
    let fill_width = (width_needed * (width corner.pattern) - (height corner.pattern)) in
    let fill_height = (height_needed * (width corner.pattern) - (height corner.pattern)) in
    let top = vrepeat corner.pattern width_needed in
    let bottom = rotate_ccw @@ rotate_ccw top in
    let left = hrepeat (rotate_ccw corner.pattern) height_needed in
    let right = rotate_ccw @@ rotate_ccw left in
    let tiled_center = expand_center ~desired_width:fill_width ~desired_height:fill_height ~pattern:center ~fill in
    assemble_rotated_borders ~left ~right ~top ~bottom ~center:tiled_center
  | Flip ->
    (* we may need to insert an extra repetition of the corner pattern,
     * since we need to have an even number to get the desired effect.
     * (otherwise we would have a 'tile' in the middle and it will probably look weird.)
     *)
    let width_needed = border_repetitions ~fencepost:0 ~center:(width center) ~side:(width corner.pattern) in
    let height_needed = border_repetitions ~fencepost:0 ~center:(height center) ~side:(height corner.pattern) in
    (* make them even numbers *)
    let width_needed = width_needed + (width_needed mod 2)
    and height_needed = height_needed + (height_needed mod 2)
    in
    let hflipped = hflip corner.pattern
    and vflipped = vflip corner.pattern
    in
    (* corners are included with bottom and top, so add an extra repetition there *)
    let horizontal_repetitions = (width_needed / 2) + 1 in
    let vertical_repetitions = height_needed / 2 in
    let top =
      vrepeat corner.pattern horizontal_repetitions <|> vrepeat hflipped horizontal_repetitions
    in
    let bottom = vflip top in
    let left = hrepeat corner.pattern vertical_repetitions <-> hrepeat vflipped vertical_repetitions in
    let right = hflip left in
    let tiled_center = expand_center ~fill
        ~desired_width:(width_needed * (width corner.pattern))
        ~desired_height:(height left) ~pattern:center in
    top
      <->
      (left <|> tiled_center <|> right)
      <->
      bottom


let fencepost_and_corner ~center:_ ~corner:_ ~fencepost:_ ~fill:_ = assert false

let side_no_fencepost ~center:_ ~corner:_ ~side:_ ~fill:_ = assert false

let side_and_fencepost ~center:_ ~corner:_ ~side:_ ~fencepost:_ ~fill:_ = assert false

let emborder ~border ~(center : Stitchy.Types.pattern) ~fill =
  let open Stitchy.Types in
  match border.side, border.fencepost with
  | None, None ->
    just_corner ~corner:border.corner ~center ~fill
  | None, Some fencepost ->
    fencepost_and_corner ~center ~corner:border.corner ~fencepost ~fill
  | Some side, None ->
    side_no_fencepost ~center ~corner:border.corner ~side ~fill
  | Some side, Some fencepost ->
    side_and_fencepost ~center ~corner:border.corner ~side ~fencepost ~fill
