open Stitchy.Types

let background =
  let doc = "color of background cloth" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

let grid_enum = [
  "14", Fourteen;
  "16", Sixteen;
  "18", Eighteen;
]

let grid =
  let doc = "Grid size of Aida cloth to use" in
  Cmdliner.Arg.(value & opt (enum grid_enum) Fourteen & info ["g"; "grid"] ~doc)

let width =
  let doc = "width for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
  Cmdliner.Arg.(value & opt int 0 & info ["w"; "width"] ~doc)

let height =
  let doc = "height for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
  Cmdliner.Arg.(value & opt int 0 & info ["h"; "height"] ~doc)

let exclude =
  let open Cmdliner.Arg in
  let thread_conv : Stitchy.DMC.Thread.t conv = conv Stitchy.DMC.Thread.(parse, pp) in
  let doc = "thread identifiers to omit from the pattern. This is useful when consuming output from a program with no support for transparency - put stitch colors you expect to be the background here." in
  Cmdliner.Arg.(value & opt_all thread_conv [] & info ["e"; "exclude"] ~doc)

let input =
  let doc = "file to read layer data from. - for stdin (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "output the assembled pattern here. - for stdout (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

let go input grid width height background exclude output =
  match Stitchy.Files.stdin_or_file input with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.layers_of_yojson json with
    | Error s -> failwith s
    | Ok layers ->
      let pattern = Assemble.stitch background width height grid exclude layers in
      Stitchy.Files.stdout_or_file (Stitchy.Types.pattern_to_yojson pattern) output

let info =
  let doc = "assemble" in
  Cmdliner.Term.info doc

let go_t = Cmdliner.Term.(const go $ input $ grid $ width $ height $ background $ exclude $ output)

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (go_t, info)
