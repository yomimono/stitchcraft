let width =
  let doc = "width of the rectangle, in number of cross-stitches" in
  Cmdliner.Arg.(value & pos 0 int 1 & info [] ~doc ~docv:"WIDTH")

let height =
  let doc = "height of the rectangle, in number of cross-stitches" in
  Cmdliner.Arg.(value & pos 1 int 1 & info [] ~doc ~docv:"HEIGHT")

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

let x =
  let doc = "offset the rectangle by x stitches from the left edge" in
  Cmdliner.Arg.(value & opt int 0 & info ["x"; "x_offset"] ~doc ~docv:"X")

let y =
  let doc = "offset the rectangle by y stitches from the top edge" in
  Cmdliner.Arg.(value & opt int 0 & info ["y"; "y_offset"] ~doc ~docv:"Y")

let thread =
  let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp) in
  let default = List.hd Stitchy.DMC.Thread.basic in
  let doc = "thread color for the stitches" in
  Cmdliner.Arg.(value & opt thread_conv default & info ["t"; "thread"] ~doc ~docv:"THREAD")

let rect height width thread bg gridsize x y =
  let substrate = Primitives.empty bg gridsize ~width:(x + width) ~height:(y + height) in
  let pattern = Primitives.rect substrate.Stitchy.Types.substrate thread x y width height in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson pattern

let () =
  let go = Cmdliner.Term.(const rect $ height $ width $ thread $ background $ gridsize $ x $ y) in
  exit @@ Cmdliner.Cmd.(eval @@ v (info "rect") go)
