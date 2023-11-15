let go input grid width height background exclude output =
  match Stitchy.Files.stdin_or_file input with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.layers_of_yojson json with
    | Error s -> failwith s
    | Ok layers ->
      let pattern = Assemble.stitch background width height grid exclude layers in
      match Stitchy.Files.stdout_or_file (Stitchy.Types.pattern_to_yojson pattern) output with
      | Error s -> failwith s
      | Ok () -> ()
