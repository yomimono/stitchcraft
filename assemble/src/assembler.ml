open Stitchy.Types

let thread_parser s =
  match Stitchy.DMC.Thread.of_string s with
  | None -> Error (`Msg (Format.asprintf "No DMC thread known matching [%s]" s))
  | Some thread -> Ok thread

let thread_printer fmt t =
  Format.fprintf fmt "%s" @@ Stitchy.DMC.Thread.to_string t

let thread_conv = Cmdliner.Arg.conv (thread_parser, thread_printer)

let thread =
  let doc = "Thread color to use for this layer (overriding any available color data in the layer itself)." in
  Cmdliner.Arg.(value & opt (some thread_conv) None & info ["t"; "thread"] ~doc)

let background =
  let doc = "color of background cloth" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

let input =
  let doc = "file to read layer data from. - for stdin (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "output the assembled pattern here. - for stdout (the default)." in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

let go input user_supplied_thread background output =
  match Stitchy.Files.stdin_or_file input with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.layer_of_yojson json with
    | Error s -> failwith s
    | Ok layer ->
      let thread = match user_supplied_thread with
        | Some t -> t
        | None ->
          match Stitchy.DMC.Thread.of_rgb layer.color with
          | None -> failwith "color embedded in this layer didn't match a known thread - try specifying a different thread"
          | Some layer_supplied_thread -> layer_supplied_thread
      in
      let pattern = Assemble.stitch thread background Fourteen layer in
      Stitchy.Files.stdout_or_file (Stitchy.Types.state_to_yojson pattern) output

let info =
  let doc = "Assemble layer information and thread specification into a pattern." in
  Cmdliner.Term.info doc

let go_t = Cmdliner.Term.(const go $ input $ thread $ background $ output)

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (go_t, info)
