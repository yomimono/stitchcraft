open Stitchy.Types

let background =
  let doc = "color of background cloth" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

let grid =
  let doc = "Grid size of Aida cloth to use" in
  Cmdliner.Arg.(value & opt int 14 & info ["g"; "grid"] ~doc)

let width =
  let doc = "width for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
  Cmdliner.Arg.(value & opt int 0 & info ["w"; "width"] ~doc)

let height =
  let doc = "height for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
  Cmdliner.Arg.(value & opt int 0 & info ["h"; "height"] ~doc)

let input =
  let doc = "file to read layer data from. - for stdin (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "output the assembled pattern here. - for stdout (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

(* TODO: don't disregard grid size here *)
let go input _grid width height background output =
  match Stitchy.Files.stdin_or_file input with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.layers_of_yojson json with
    | Error s -> failwith s
    | Ok layers ->
      let pattern = Assemble.stitch background width height Fourteen layers in
      Stitchy.Files.stdout_or_file (Stitchy.Types.pattern_to_yojson pattern) output

let info =
  let doc = "Assemble layer information and substrate specification into a pattern. NB no attempt to handle backstitch, french knots, etc is made." in
  Cmdliner.Term.info doc

let go_t = Cmdliner.Term.(const go $ input $ grid $ width $ height $ background $ output)

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (go_t, info)
