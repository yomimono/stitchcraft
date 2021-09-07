let width =
  let doc = "width of the empty grid, in number of (non-) cross-stitches" in
  Cmdliner.Arg.(value & pos 0 int 1 & info [] ~doc ~docv:"WIDTH")

let height =
  let doc = "height of the empty grid, in number of (non-) cross-stitches" in
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

let empty width height bg gridsize =
  let pattern = Primitives.empty bg gridsize ~width ~height in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson pattern

let empty_t = Cmdliner.Term.info "empty"
  
let () =
  let go = Cmdliner.Term.(const empty $ width $ height $ background $ gridsize) in
  Cmdliner.Term.(exit @@ eval (go, empty_t))
