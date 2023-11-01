let piece x_off y_off width height file =
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
    let submap = Stitchy.Types.submap ~x_off ~y_off ~width ~height pattern in
    Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson submap
