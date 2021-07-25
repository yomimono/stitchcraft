open Stitchy.Types

let find_layer pattern thread stitch = 
  List.partition (fun (layer : layer) ->
      0 = Stitchy.DMC.Thread.compare thread layer.thread &&
      equal_stitch stitch layer.stitch
    ) pattern.layers

let add_stitch pattern thread stitch (x, y) =
  match find_layer pattern thread stitch with
  | [], layers ->
    let new_layer = { thread; stitch; stitches = CoordinateSet.singleton (x, y) } in
    { pattern with layers = new_layer :: layers}
  | l::_, layers ->
    let layer_with_new_stitch = {l with stitches = CoordinateSet.add (x, y) l.stitches} in
    { pattern with layers = layer_with_new_stitch::layers }

let remove_stitch pattern (x, y) =
  let remove_stitch_from_layer (x, y) (l : layer) =
    {l with stitches = CoordinateSet.remove (x, y) l.stitches}
  in
  (* don't be clever, just ask to remove this coordinate from every layer *)
  {pattern with layers = List.map (remove_stitch_from_layer (x, y)) pattern.layers}
