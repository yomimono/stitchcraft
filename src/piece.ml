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
      (* TODO: fuck backstitch *)
  | Ok { substrate; layers; _ } ->
    let displace (m, n) = (m - x_off), (n - y_off) in
    let l = Stitchy.Types.submap ~x_off ~y_off ~width ~height layers in
    let substrate = Stitchy.Types.({substrate with max_x = max 0 @@ width - 1;
                                    max_y = max 0 @@ height - 1;
                    }) in
    let p = Stitchy.Types.({ substrate; layers = l; backstitch_layers = [] }) |>
      (Stitchy.Operations.transform_all_stitches ~f:displace) in
    Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson p
