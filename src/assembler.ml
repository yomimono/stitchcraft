let go input grid width height background exclude output =
  match Util.stdin_or_file input with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.layers_of_yojson json with
    | Error s -> failwith s
    | Ok layers ->
      let pattern = Assemble.stitch background width height grid exclude layers in
      match Util.pattern_to_output pattern output with
      | Error s -> failwith s
      | Ok () -> ()
