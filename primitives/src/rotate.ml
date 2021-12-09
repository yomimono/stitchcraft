let file =
  (* for now, only 90 degrees counterclockwise for you *)
  let doc = "pattern to rotate 90 degrees counterclockwise. - for stdin" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc ~docv:"FILE")

let rotate file =
  let json = function
    | s when 0 = String.compare s "-" -> begin
        try Yojson.Safe.from_channel stdin with _exn -> failwith "couldn't understand input"
      end
    | src ->
      try Yojson.Safe.from_file src with _exn -> failwith "couldn't read file"
  in
  match Stitchy.Types.pattern_of_yojson (json file) with
  | Error e -> failwith @@ Printf.sprintf "couldn't parse input file: %s" e
  | Ok pattern ->
    Stitchy.Operations.rotate_ccw pattern |> Stitchy.Types.pattern_to_yojson |> Yojson.Safe.to_channel stdout

let info = Cmdliner.Term.info "rotate" ~doc:"rotate a pattern counterclockwise"

let () = Cmdliner.(Term.exit @@ Term.eval (Term.(const rotate $ file), info))
