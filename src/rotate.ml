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
