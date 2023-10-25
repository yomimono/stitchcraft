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
