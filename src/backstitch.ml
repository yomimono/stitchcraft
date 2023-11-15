let bs background grid thread l =
  let open Stitchy.Types in
  let max_x, max_y = List.fold_left (fun (max_x, max_y) ((src_x, src_y), (dst_x, dst_y))->
      let new_max_x = max max_x @@ (max src_x dst_x) - 1
      and new_max_y = max max_y @@ (max src_y dst_y) - 1
      in
      (new_max_x, new_max_y)
    ) (0, 0) l
  in
  let substrate = {
    background;
    grid;
    max_x;
    max_y;
  } in
  let backstitch_layers = [{
      thread;
      stitches = SegmentSet.of_list l;
    }] in
  let pattern = {substrate; backstitch_layers; layers = []} in
  Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_channel stdout
