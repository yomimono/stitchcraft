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


(** [tile pattern ~dimensions ~mask_dimensions] fills an area of `dimensions` size with
 * tiling repetitions of `pattern`, except for any dimensions in `mask_dimensions`,
 * which are left unfilled but do not interrupt the tiling. *)
let tile pattern ~(dimensions : dimensions) ~mask_dimensions =
  let open Stitchy.Types in
  Stdlib.Format.eprintf "filling %a, masking off %a" pp dimensions (Fmt.list pp) mask_dimensions;
  let row pattern ~(dimensions : dimensions) =
    let vrepetitions =
      if dimensions.width mod (pattern.substrate.max_x + 1) = 0 then
        dimensions.width / (pattern.substrate.max_x + 1)
      else dimensions.width / (pattern.substrate.max_x + 1) + 1
    in
    let r = Stitchy.Operations.vrepeat pattern vrepetitions in
    {r with substrate = {r.substrate with max_x = dimensions.width - 1}}
  in
  let hrepetitions =
    if dimensions.height mod (pattern.substrate.max_y + 1) = 0 then
      dimensions.height / (pattern.substrate.max_y + 1)
    else dimensions.height / (pattern.substrate.max_y + 1) + 1
  in
  let r = Stitchy.Operations.hrepeat (row pattern ~dimensions) hrepetitions in
  let unmasked = {r with substrate = {r.substrate with max_y = dimensions.height - 1}} in
  let shifted = displace_pattern (RightAndDown (dimensions.x_off, dimensions.y_off)) unmasked in
  let masks : CoordinateSet.t =
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
  {shifted with layers =
           List.map (fun (layer : layer) -> {layer with stitches = CoordinateSet.diff layer.stitches masks})
             shifted.layers
  }

(* TODO: this definitely needs a better name *)
(* this is the guilloche-style corner-plus repeating border *)
let better_embellish ~fill ~corner ~top ~center =
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
  let corner_long_side = corner.substrate.max_x + 1 in
  let corner_short_side = corner.substrate.max_y + 1 in
  let top_border_width = top_border.substrate.max_x + 1 in
  let borders = [
    (* top border just needs to move over to make room for the upper left-hand corner *)
    displace_pattern (Right corner_long_side) top_border;
    (* right-hand border needs to move to the right side,
     * and also to move down to make room for the upper right-hand corner *)
    displace_pattern (RightAndDown (corner_long_side + top_border_width,
                                    corner_long_side))
      right_border; 
    (* bottom border needs to move to the right to make room for the lower left-
     * hand corner (on the short side), and to go to the bottom of the pattern *)
    displace_pattern (RightAndDown (corner_short_side,
                                    corner_long_side + left_border.substrate.max_y + 1))
      bottom_border;
    (* the left border is already on the left side, and just needs to shift down
     * to make room for the upper left-hand corner's short side *)
    displace_pattern (Down corner_short_side) left_border;
  ] in
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
                                    corner_long_side + left_border.substrate.max_y + 1));
    (* lower left is rotated counter-clockwise 90 degrees,
     * then shifted down past the short end of the upper-left corner and the
     * whole left-side border. *)
    rotate_ccw corner |>
    displace_pattern (Down (corner_short_side + left_border.substrate.max_y + 1));
  ] in
  let substrate = { center.Stitchy.Types.substrate with
                    max_x = corner.substrate.max_x + 1 +
                            top_border.substrate.max_x + 1 +
                            corner.substrate.max_y;
                    max_y = left_border.substrate.max_y + 1 +
                            corner.substrate.max_x + 1 +
                            corner.substrate.max_y;
                  } in
  let left_padding, top_padding = 
    (* the center will be smaller than the borders,
     * so not only does it need to move down and to the right
     * to avoid the borders, it also needs to (potentially)
     * move down and to the right to center itself within them. *)
    ((substrate.max_x - center.substrate.max_x) / 2),
    ((substrate.max_y - center.substrate.max_y) / 2)
  in
  let center_shifted = displace_pattern (RightAndDown (left_padding, top_padding)) center in
  let center_mask = {x_off = left_padding; y_off = top_padding; width = center.substrate.max_x + 1; height = center.substrate.max_y + 1 } in
  let (center_dimensions : dimensions) = {x_off = top_border.substrate.max_y + 1;
                                          y_off = (left_border.substrate.max_x + 1);
                                          width = center_shifted.substrate.max_x + 1;
                                          height = center_shifted.substrate.max_y + 1} in
  let center_fill = tile fill ~dimensions:center_dimensions ~mask_dimensions:[center_mask] in
  List.fold_left (merge_patterns ~substrate) center_shifted (center_fill :: corners @ borders)

(* this is the simpler, corners-and-repeating-motif kind of repeating border *)
let embellish ~rotate_corners ~center ~corner ~top ~fencepost =
  let open Stitchy.Types in
  let open Stitchy.Operations in
  let side = rotate_ccw top in
  let fencepost_w = match fencepost with
    | None -> 0
    | Some fencepost -> fencepost.substrate.max_x + 1
  in
  let horiz_border_reps = Compose.border_repetitions
      ~fencepost:fencepost_w
      ~center:(center.substrate.max_x + 1)
      ~side:(top.substrate.max_x + 1)
  in
  let vert_border_reps = Compose.border_repetitions
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
