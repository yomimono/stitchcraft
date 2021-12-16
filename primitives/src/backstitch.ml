let src_x =
  let doc = "backstitch source x coordinate" in
  Cmdliner.Arg.(value & pos 0 int 0 & info [] ~doc ~docv:"SRC_X")

let src_y =
  let doc = "backstitch source y coordinate" in
  Cmdliner.Arg.(value & pos 1 int 0 & info [] ~doc ~docv:"SRC_Y")

let dst_x =
  let doc = "backstitch destination x coordinate" in
  Cmdliner.Arg.(value & pos 2 int 0 & info [] ~doc ~docv:"DST_X")

let dst_y =
  let doc = "backstitch destination y coordinate" in
  Cmdliner.Arg.(value & pos 3 int 0 & info [] ~doc ~docv:"DST_Y")

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

let bs background grid thread src_x src_y dst_x dst_y =
  let open Stitchy.Types in
  let substrate = {
    background;
    grid;
    max_x = max 0 @@ (max src_x dst_x) - 1;
    max_y = max 0 @@ (max src_y dst_y) - 1;
  } in
  let segment : segment = (src_x, src_y), (dst_x, dst_y) in
  let backstitch_layers = [{
      thread;
      stitches = SegmentSet.singleton segment;
    }] in
  let pattern = {substrate; backstitch_layers; layers = []} in
  Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_channel stdout

let bs_t = Cmdliner.Term.info "backstitch"

let () =
  Cmdliner.Term.(exit @@ eval (const bs $ background $ gridsize $ thread $ src_x $ src_y $ dst_x $ dst_y, bs_t))
