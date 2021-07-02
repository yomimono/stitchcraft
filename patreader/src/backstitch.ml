let add_backstitch (layers : Stitchy.Types.backstitch_layer list) thread stitch =
  let eq_layers_disregarding_stitch_set layer thread =
    Stitchy.DMC.Thread.compare layer.Stitchy.Types.thread thread = 0
  in
  match List.partition (fun layer ->
      eq_layers_disregarding_stitch_set layer thread) layers with
  | layer::_, other_layers ->
    let stitches =
      Stitchy.Types.SegmentSet.add stitch layer.stitches in
    Ok ({ layer with stitches } :: other_layers)
  | [], other_layers ->
    let layer = Stitchy.Types.{
        thread; stitches = SegmentSet.singleton stitch
      } in
    Ok (layer :: other_layers)

(* the position numbers *appear* to be:
 1  2  3
 4  5  6
 7  8  9

 where 1, 3, 7, and 9 are the vertices of the grid lines
   and 5 is the center of the warp/weft overlap. *)

(* In our scheme, this gives a clear many:one mapping to our grid:
 * (0, 0):1 -> (0, 0)
 * (0, 0):3 -> (1, 0)
 * (0, 0):7 -> (0, 1)
 * (0, 0):9 -> (1, 1)
 *
 * (0, 1):1 -> (1, 0)
 * (0, 1):3 -> (2, 0)
 * (0, 1):7 -> (1, 1)
 * (0, 1):9 -> (2, 1)
 *
 * ...and so on. *)

let segment_of_backstitch _backstitch =
  Error (`Msg "nah")

let layers_of_backstitches threads backstitches =
  let one_backstitch acc backstitch =
    match acc with
    | Error e -> Error e
    | Ok layers ->
      match (List.nth threads (backstitch.Patreader.color_index - 1)) with
      | None -> Error (`Msg "unknown thread")
      | Some thread ->
        match segment_of_backstitch backstitch with
        | Error (`Msg s) ->
          Format.eprintf "%s\n%!" s;
          acc
        | Ok segment ->
          add_backstitch layers thread segment
  in
  List.fold_left one_backstitch (Ok []) backstitches
