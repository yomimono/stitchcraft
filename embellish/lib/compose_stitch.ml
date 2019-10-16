(* quite a bit of this logic will be similar to the notty concatenation logic *)
open Stitchy.Types

let displace_down ~amount ((x, y) : Block.t) =
  x, (y + 1 + amount)

let displace_right ~amount ((x, y) : Block.t) =
  (x + 1 + amount), y

let hcat_with_substrate substrate upper lower =
  let new_max_y = upper.substrate.max_y + lower.substrate.max_y + 1
  and new_max_x = max upper.substrate.max_x lower.substrate.max_x
  in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let stitches = Stitchy.Types.BlockMap.fold (fun (x, y) v m ->
      BlockMap.add (displace_down ~amount:upper.substrate.max_y (x, y)) v m
    ) lower.stitches upper.stitches in
  { substrate; stitches }

(** [hcat upper lower] horizontally concatenates two patterns.
    If the substrates differ in background color, grid, or block size,
    the upper pattern's substrate will be used. *)
let hcat upper lower =
  hcat_with_substrate upper.substrate upper lower

let vcat_with_substrate substrate left right =
  let new_max_x = left.substrate.max_x + right.substrate.max_x + 1
  and new_max_y = max left.substrate.max_y right.substrate.max_y
  in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let stitches = Stitchy.Types.BlockMap.fold (fun (x, y) v m ->
      BlockMap.add (displace_right ~amount:left.substrate.max_x (x, y)) v m
    ) right.stitches left.stitches in
  { substrate; stitches }

(** [vcat left right] vertically concatenates two patterns.
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

let embellish ~center ~corner ~side =
  let open Compose in
  let horiz_border_reps = border_repetitions
      ~center:(center.substrate.max_x + 1)
      ~side:(side.substrate.max_x + 1)
  in
  let vert_border_reps = border_repetitions
      ~center:(center.substrate.max_y + 1)
      ~side:(side.substrate.max_y + 1)
  in
  (* how much extra whitespace should the center image get? *)
  let _extra_x = (horiz_border_reps * (side.substrate.max_x + 1)) - (center.substrate.max_x + 1) 
  and _extra_y = (vert_border_reps * (side.substrate.max_x + 1)) - (center.substrate.max_y + 1)
  in
  let _empty base_substrate width height =
    let substrate = {base_substrate with max_x = width - 1; max_y = height - 1} in
    let stitches = Stitchy.Types.BlockMap.empty in
    { substrate; stitches }
  in
  let (<->) = hcat in
  let (<|>) = vcat in
  let side_border = hrepeat side vert_border_reps in
  let top_border = vrepeat side horiz_border_reps in
  (corner <|> top_border <|> corner)
  <->
  ((side_border <|> center) <|> side_border)
  <->
  (corner <|> top_border <|> corner)
