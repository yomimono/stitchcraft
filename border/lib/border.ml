open Stitchy.Operations

type dimensions = {
  x_off : int;
  y_off : int;
  w: int;
  h: int;
}

let pp fmt {x_off; y_off; w; h} =
  Format.fprintf fmt "%d x %d starting at %d , %d" w h x_off y_off

let within ~x ~y {x_off; y_off; w; h} =
  x_off <= x && x < (x_off + w) &&
  y_off <= y && y < (y_off + h)

let max3 a b c : int = max (max a b) (max b c)
let make_odd n = if n mod 2 = 0 then n + 1 else n

let border_repetitions ~fencepost ~center ~side =
  match center mod (side + fencepost) with
  (* since we need to insert another fencepost anyway, leftovers <= the size of the last fencepost are nothing to worry about *)
  | w when w <= fencepost -> center / (side + fencepost)
  | _ -> (* too much space left over; add another repetition *)
    center / (side + fencepost) + 1

let backstitch_in (src, dst) dim =
  (dim.x_off <= (fst src) && (fst src) <= dim.x_off + dim.w &&
   dim.y_off <= (snd src) && (snd src) <= dim.y_off + dim.h) &&
  (dim.x_off <= (fst dst) && (fst dst) <= dim.x_off + dim.w &&
   dim.y_off <= (snd dst) && (snd dst) <= dim.y_off + dim.h)

(** [tile pattern ~dimensions ~mask_dimensions] fills an area of `dimensions` size with
 * tiling repetitions of `pattern`, except for any dimensions in `mask_dimensions`,
 * which are left unfilled but do not interrupt the tiling. *)
let tile pattern ~(dimensions : dimensions) ~mask_dimensions =
  let open Stitchy.Types in
  let row pattern ~(dimensions : dimensions) =
    let vrepetitions =
      if dimensions.w mod (pattern.substrate.max_x + 1) = 0 then
        dimensions.w / (pattern.substrate.max_x + 1)
      else dimensions.w / (pattern.substrate.max_x + 1) + 1
    in
    let r = Stitchy.Operations.vrepeat pattern vrepetitions in
    {r with substrate = {r.substrate with max_x = (dimensions.w - 1)}}
  in
  let hrepetitions =
    if dimensions.h mod (pattern.substrate.max_y + 1) = 0 then
      dimensions.h / (pattern.substrate.max_y + 1)
    else dimensions.h / (pattern.substrate.max_y + 1) + 1
  in
  let r = Stitchy.Operations.hrepeat (row pattern ~dimensions) hrepetitions in
  let unmasked = {r with substrate = {r.substrate with max_y = dimensions.h - 1}} in
  let shifted = displace_pattern (RightAndDown (dimensions.x_off, dimensions.y_off)) unmasked in
  let stitch_masks : CoordinateSet.t =
    List.fold_left
      (fun so_far (mask : dimensions) ->
         let xs = List.init mask.w (fun n -> n + mask.x_off) in
         let ys = List.init mask.h (fun n -> n + mask.y_off) in
         List.fold_left (fun cs x ->
             List.fold_left (fun cs y ->
                 CoordinateSet.add (x, y) cs
               ) cs ys)
           so_far xs) CoordinateSet.empty mask_dimensions
  in
  let max_x = dimensions.w + dimensions.x_off - 1
  and max_y = dimensions.h + dimensions.y_off - 1
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

let assemble_simple_borders ~left ~right ~top ~bottom ~center =
  top
  <->
  (left <|> center <|> right)
  <->
  bottom

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
                                  w = desired_width;
                                  h = desired_height; } in
  let x_off, y_off =
    fst @@ halfway_with_left_bias ~whole:desired_width ~part:(width pattern),
    fst @@ halfway_with_left_bias ~whole:desired_height ~part:(height pattern)
  in
  let mask_dimensions = [{
      w = (width pattern);
      h = (height pattern);
      x_off;
      y_off;
    }] in
  let background = tile fill ~dimensions ~mask_dimensions in
  (* now drop the pattern in the masked-off area *)
  let displacement = RightAndDown (x_off, y_off) in
  let shifted_pattern = displace_pattern displacement pattern in
  merge_patterns ~substrate:(background.substrate) background shifted_pattern

let just_corner ~(center : Stitchy.Types.pattern) ~corner ~fill ~min_width ~min_height =
  let open Stitchy.Types in
  let min_width = max min_width @@ width center
  and min_height = max min_height @@ height center
  in
  match corner.transformation with
  | Nothing -> begin
      (* easiest case I guess? *)
    let width_needed = border_repetitions ~fencepost:0 ~side:(width corner.pattern) ~center:min_width in
    let height_needed = border_repetitions ~fencepost:0 ~side:(height corner.pattern) ~center:min_height in
    let fill_width = (width_needed * (width corner.pattern)) in
    let fill_height = (height_needed * (height corner.pattern)) in
    (* top and bottom are the same, as are left and right, so just use top/left for both *)
    let top = vrepeat corner.pattern (width_needed + 2) in
    let left = hrepeat corner.pattern height_needed in
    let tiled_center = expand_center ~desired_width:fill_width ~desired_height:fill_height ~pattern:center ~fill in
    assemble_simple_borders ~top ~left ~bottom:top ~right:left ~center:tiled_center
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
    assemble_simple_borders ~left ~right ~top ~bottom ~center:tiled_center

let fencepost_and_corner ~center ~corner ~fencepost ~fill ~min_width ~min_height =
  let open Stitchy.Types in
  (* since we allow separate fencepost transformations, this isn't as easy as it might seem...
   * *)
  match corner.transformation, fencepost.transformation with
  | Turn, Turn ->
    (* in this case we can just weld the fencepost onto the end of the corner *)
    let corner = {corner with pattern = corner.pattern <|> fencepost.pattern } in
    just_corner ~center ~corner ~fill ~min_width ~min_height
  | Nothing, Nothing ->
    let width_needed = border_repetitions ~fencepost:(width fencepost.pattern)
        ~side:(width corner.pattern) ~center:(width center) in
    let height_needed = border_repetitions ~fencepost:(height fencepost.pattern)
        ~side:(height corner.pattern) ~center:(height center) in
    let top =
      let c_and_fp = corner.pattern <|> fencepost.pattern in
      (vrepeat c_and_fp (width_needed + 1)) <|> corner.pattern
    in
    let left =
      let c_and_fp = corner.pattern <-> fencepost.pattern in
      (hrepeat c_and_fp height_needed) <-> fencepost.pattern
    in
    let desired_width = (width top) - (2 * (width corner.pattern)) in
    let desired_height = (height left) in
    let tiled_center  = expand_center ~desired_width ~desired_height ~pattern:center ~fill in
    assemble_simple_borders ~left ~top ~right:left ~bottom:top ~center:tiled_center
  | Flip, Flip ->
    (* don't try to make anything in particular centered, since it's gonna
     * look weird either way *)
    (* TODO I think it's more likely that we'll want the fencepost centered,
     * since they tend to be smaller & might have different symmetry *)
    let width_needed =
      border_repetitions ~fencepost:(width fencepost.pattern)
        ~side:(width corner.pattern) ~center:(width center) in
    let height_needed =
      border_repetitions ~fencepost:(height fencepost.pattern)
        ~side:(height corner.pattern) ~center:(height center) in
    let top =
      let left = (fencepost.pattern <|> corner.pattern) in
      let right = (hflip fencepost.pattern) <|> (hflip corner.pattern) in
      corner.pattern <|> 
      (vrepeat left @@ width_needed / 2 + (width_needed mod 2))
      <|>
      (vrepeat right @@ (width_needed / 2) + 1)
    in
    let bottom = vflip top in
    let left =
      let top = (fencepost.pattern <-> corner.pattern) in
      let bottom = (vflip fencepost.pattern) <-> (vflip corner.pattern) in
      (hrepeat top @@ (height_needed / 2) + (height_needed mod 2))
      <->
      (hrepeat bottom @@ height_needed / 2)
      <->
      (vflip fencepost.pattern)
    in
    let right = hflip left in
    let desired_width = (width top) - ((width corner.pattern) - 2) in
    let desired_height = (height left) in
    let tiled_center = expand_center ~desired_width ~desired_height ~fill ~pattern:center in
    assemble_simple_borders ~left ~right ~top ~bottom ~center:tiled_center
  | Nothing, Turn ->
    (* on the top and bottom, we'll be using fencepost's width;
     * on the left and right, we'll rotate it, so it's its height that matters *)
    let width_needed = border_repetitions ~fencepost:(width fencepost.pattern) ~side:(width corner.pattern) ~center:(width center) in
    let height_needed = border_repetitions ~fencepost:(height fencepost.pattern) ~side:(width corner.pattern) ~center:(height center) in
    (* border_repetitions gives us the number of repetitions of `side` we'll need, assuming
     * that we've fenceposted them with `fencepost`, so to cover all of `top`
     * we need one more `corner`, the result of `border_repetitions` repetitions of fencepost + side, another fencepost, and one more corner *)
    let top_component = corner.pattern <|> fencepost.pattern in
    let top = top_component <|> vrepeat top_component width_needed <|> corner.pattern in
    let left_fencepost = rotate_ccw fencepost.pattern in
    let left_component = left_fencepost <-> corner.pattern in
    let left = hrepeat left_component height_needed <-> left_fencepost in
    (* bottom isn't just a rotated top, since the corner image isn't supposed to change *)
    let bottom_fencepost = rotate_ccw @@ rotate_ccw fencepost.pattern in
    let bottom_component = corner.pattern <|> bottom_fencepost in
    let bottom = bottom_component <|> vrepeat bottom_component width_needed <|> corner.pattern in
    let right_fencepost = (rotate_ccw @@ rotate_ccw @@ rotate_ccw fencepost.pattern) in
    let right_component = right_fencepost <-> corner.pattern in
    let right = hrepeat right_component height_needed <-> right_fencepost in

    let desired_width = (width top) - (2 * (width corner.pattern)) in
    let desired_height = (height left) in
    let tiled_center  = expand_center ~desired_width ~desired_height ~pattern:center ~fill in
    assemble_simple_borders ~left ~right ~top ~bottom ~center:tiled_center
  | Nothing, Flip ->
    (* this is also a pretty weird thing to do, but OK *)
    let width_needed = border_repetitions ~fencepost:(width fencepost.pattern) ~side:(width corner.pattern) ~center:(width center) in
    let height_needed = border_repetitions ~fencepost:(height fencepost.pattern) ~side:(height corner.pattern) ~center:(height center) in
    (* we need to make sure a fencepost isn't smack in the middle, so we need to make sure
     * there are an odd number of repetitions of the corner element on each side *)
    let width_needed = make_odd width_needed
    and height_needed = make_odd height_needed
    in
    let top =
      let c_and_plain = corner.pattern <|> fencepost.pattern in
      let hflipped_and_c = (hflip fencepost.pattern) <|> corner.pattern in
      c_and_plain <|> (vrepeat c_and_plain (width_needed / 2))
      <|> corner.pattern
      <|> (vrepeat hflipped_and_c ((width_needed / 2) + 1)) <|> corner.pattern
    in
    (* can't just vflip top for the bottom, since the corner elements shouldn't change *)
    let bottom =
      let left_side_component = corner.pattern <|> (vflip fencepost.pattern) in
      let right_side_component = (vflip @@ hflip fencepost.pattern) <|> corner.pattern in
      vrepeat left_side_component ((width_needed / 2) + 1)
       <|> corner.pattern
       <|> (vrepeat right_side_component ((width_needed / 2) + 1))
    in
    let make_side ~top_component ~bottom_component =
      (hrepeat top_component (height_needed / 2)) <->
      corner.pattern <->
      (hrepeat bottom_component (height_needed / 2))
    in
    let left =
      let top_component = corner.pattern <-> fencepost.pattern in
      let bottom_component = (vflip fencepost.pattern) <-> corner.pattern in
      make_side ~top_component ~bottom_component
    in
    let right =
      let top_component = corner.pattern <-> (hflip fencepost.pattern) in
      let bottom_component = (vflip @@ hflip fencepost.pattern) <-> corner.pattern
      in
      make_side ~top_component ~bottom_component
    in
    let desired_width = (width top) - (2 * (width corner.pattern)) in
    let desired_height = (height right) in
    let tiled_center = expand_center ~desired_width ~desired_height ~pattern:center ~fill in
    assemble_simple_borders ~left ~right ~top ~bottom ~center:tiled_center
  | Turn, Flip -> assert false
  | Turn, Nothing -> assert false
  | Flip, Nothing -> assert false
  | Flip, Turn -> assert false

let side_no_fencepost ~center ~corner ~side ~fill ~min_width ~min_height =
  let open Stitchy.Types in
  match corner.transformation, side.transformation with
  | Nothing, Nothing ->
    let min_width = max min_width @@ width center
    and min_height = max min_height @@ height center
    in
    let treps = border_repetitions ~fencepost:0 ~side:(width side.pattern) ~center:min_width in
    let lreps = border_repetitions ~fencepost:0 ~side:(height side.pattern) ~center:min_height in
    let top = corner.pattern <|> vrepeat side.pattern treps <|> corner.pattern in
    let left = hrepeat side.pattern lreps in
    let center = expand_center ~desired_width:(width @@ vrepeat side.pattern treps)
        ~desired_height:(height left) ~pattern:center ~fill in
    assemble_simple_borders ~top ~left ~center ~right:left ~bottom:top
  | Turn, Turn ->
    let corner_w = (width corner.pattern) in
    let corner_h = (height corner.pattern) in
    let side_w = (width side.pattern) in
    (* "border repetitions" gets a little tricky here. *)
    (* we always have at least one corner iteration. This might be wide enough
     * to cover some of the width we need to make;
     * the amount of 'overhang' is the corner's width minus its length. *)
    (* It's correct that this can be negative. If the corner is taller than it is wide,
     * the side pattern will have to cover extra area. *)
    let overhang = corner_w - corner_h in
    let min_width = max min_width @@ width center
    and min_height = max min_height @@ height center
    in
    let width_needed =
      border_repetitions ~fencepost:0 ~side:(side_w) ~center:(min_width - overhang)
    in
    let height_needed =
      border_repetitions ~fencepost:0 ~side:(side_w) ~center:(min_height - overhang)
    in
    (* width_needed and height_needed have removed the "overhang" of a corner,
     * so their numbers reflect sides. *)
    (* unfortunately we need some more arithmetic to reconstruct the width of the center
     * thing. *)
    let fill_width = width_needed * (width side.pattern) + overhang in
    let fill_height = height_needed * (width side.pattern) + overhang in
    let center = expand_center ~desired_width:fill_width ~desired_height:fill_height
        ~pattern:center ~fill in
    let top = corner.pattern <|> (vrepeat side.pattern width_needed) in
    let bottom = rotate_ccw @@ rotate_ccw top in
    let left = (hrepeat (rotate_ccw side.pattern) height_needed) <-> (rotate_ccw corner.pattern) in
    let right = rotate_ccw @@ rotate_ccw left in
    assemble_rotated_borders ~left ~right ~top ~bottom ~center
      
  | _, _ ->
    assert false

let side_and_fencepost ~center ~corner ~side ~fencepost ~fill ~min_width ~min_height =
  let open Stitchy.Types in
  let min_width = max min_width @@ width center
  and min_height = max min_height @@ height center
  in
  match corner.transformation, side.transformation, fencepost.transformation with
  | Nothing, Nothing, Nothing ->
    let fp_w, fp_h = (width fencepost.pattern), (height fencepost.pattern) in
    let treps = border_repetitions ~fencepost:fp_w ~side:(width side.pattern) ~center:min_width in
    let lreps = border_repetitions ~fencepost:fp_h ~side:(height side.pattern) ~center:min_height in
    let top =
      let top_component = side.pattern <|> fencepost.pattern in
      corner.pattern <|>
      fencepost.pattern <|>
      (vrepeat top_component treps) <|>
      corner.pattern
    in
    let left =
      let left_component = fencepost.pattern <-> side.pattern in
      (hrepeat left_component lreps) <-> fencepost.pattern
    in
    let center = expand_center ~desired_width:((width top) - ((width corner.pattern) * 2)) ~desired_height:(height left) ~pattern:center ~fill in
    assemble_simple_borders ~top ~left ~center ~right:left ~bottom:top
  | _, _, _ -> assert false

let emborder ~border ~(center : Stitchy.Types.pattern) ~fill ~min_width ~min_height =
  let open Stitchy.Types in
  match border.side, border.fencepost with
  | None, None ->
    just_corner ~corner:border.corner ~center ~fill ~min_width ~min_height
  | None, Some fencepost ->
    fencepost_and_corner ~center ~corner:border.corner ~fencepost ~fill ~min_width ~min_height
  | Some side, None ->
    side_no_fencepost ~center ~corner:border.corner ~side ~fill ~min_width ~min_height
  | Some side, Some fencepost ->
    side_and_fencepost ~center ~corner:border.corner ~side ~fencepost ~fill ~min_width ~min_height
