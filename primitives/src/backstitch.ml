let segment =
  let point = Cmdliner.Arg.(pair int int) in
  Cmdliner.Arg.(pair ~sep:'/' point point)

let segments =
  let doc = "list of backstitches as pairs of sources and destinations on the line grid" in
  Cmdliner.Arg.(value & pos_all segment [] & info [] ~doc ~docv:"SEGMENTS")

let thread =
  let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp) in
  let default = List.hd Stitchy.DMC.Thread.basic in
  let doc = "thread color for the stitches" in
  Cmdliner.Arg.(value & opt thread_conv default & info ["t"; "thread"] ~doc ~docv:"THREAD")

let background =
  let doc = "background color" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

let grid_converter : (string * Stitchy.Types.grid) list = [
  "14", Fourteen;
  "16", Sixteen;
  "18", Eighteen;
]

let gridsize =
  let doc = "size of aida cloth grid" in
  Cmdliner.Arg.(value & opt (enum grid_converter) Stitchy.Types.Fourteen & info ["g"; "gridsize"] ~doc)

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

let bs_t = Cmdliner.Cmd.info "backstitch"

let () = exit @@
  Cmdliner.(Cmd.eval @@ Cmd.v bs_t @@ Term.(const bs $ background $ gridsize $ thread $ segments))
