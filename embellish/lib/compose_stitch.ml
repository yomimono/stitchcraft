(* quite a bit of this logic will be similar to the notty concatenation logic *)
open Stitchy.Types

let displace_down ~amount ((x, y) : Block.t) =
  x, (y + amount)

let displace_right ~amount ((x, y) : Block.t) =
  (x + amount), y

let hpadding image1 image2 =
  let bigger, smaller =
    if image1.substrate.max_x > image2.substrate.max_x then image1, image2 else image2, image1
  in
  if bigger.substrate.max_x = smaller.substrate.max_x then 0, 0
  else begin
    let difference = bigger.substrate.max_x - smaller.substrate.max_x in
    if difference mod 2 = 0 then (difference / 2), (difference / 2)
    else (difference / 2), (difference / 2 + 1)
  end

let vpadding image1 image2 =
  let bigger, smaller =
    if image1.substrate.max_y > image2.substrate.max_y then image1, image2 else image2, image1
  in
  if bigger.substrate.max_y = smaller.substrate.max_y then 0, 0
  else begin
    let difference = bigger.substrate.max_y - smaller.substrate.max_y in
    if difference mod 2 = 0 then (difference / 2), (difference / 2)
    else (difference / 2), (difference / 2 + 1)
  end

let hcat_with_substrate substrate upper lower =
  let new_max_y = upper.substrate.max_y + lower.substrate.max_y + 1
  and new_max_x = max upper.substrate.max_x lower.substrate.max_x
  in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let stitches = Stitchy.Types.BlockMap.fold (fun (x, y) v m ->
      BlockMap.add (displace_down ~amount:(upper.substrate.max_y + 1) (x, y)) v m
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
  let top_pad, _ = vpadding left right in
  let substrate = { substrate with max_x = new_max_x; max_y = new_max_y } in
  let stitches = BlockMap.fold (fun (x, y) v m ->
      BlockMap.add (displace_right ~amount:(left.substrate.max_x + 1) (x, y)) v m
    ) right.stitches BlockMap.empty in
  let stitches = BlockMap.fold (fun (x, y) v m ->
      BlockMap.add (displace_down ~amount:top_pad (x, y)) v m
    ) left.stitches stitches in
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

let (<->) = hcat
let (<|>) = vcat

let empty base_substrate max_x max_y =
  let substrate = {base_substrate with max_x; max_y } in
  let stitches = BlockMap.empty in
  { substrate; stitches }

let pad_horizontal center left right =
  match left, right with
  | 0, 0 -> center
  | n, 0 -> (empty center.substrate n center.substrate.max_y) <|> center
  | 0, n -> center <|> (empty center.substrate n center.substrate.max_y)
  | l, r -> (empty center.substrate l center.substrate.max_y) <|> center <|>
            (empty center.substrate r center.substrate.max_y)

let pad_vertical center top bottom =
  match top, bottom with
  | 0, 0 -> center
  | n, 0 -> (empty center.substrate center.substrate.max_x n) <-> center
  | 0, n -> center <-> (empty center.substrate center.substrate.max_x n)
  | l, r -> (empty center.substrate center.substrate.max_x l) <-> center <->
            (empty center.substrate center.substrate.max_x r)


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
  let side_border = hrepeat side vert_border_reps in
  let top_border = vrepeat side horiz_border_reps in
  let left_pad, right_pad = hpadding top_border center
  and top_pad, bottom_pad = vpadding side_border center
  in
  let center = pad_horizontal center left_pad right_pad in
  let center = pad_vertical center top_pad bottom_pad in
  (corner <|> top_border <|> corner)
  <->
  ((side_border <|> center) <|> side_border)
  <->
  (corner <|> top_border <|> corner)
