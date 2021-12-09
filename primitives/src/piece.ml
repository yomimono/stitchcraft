let file =
  let doc = "pattern to take from. - for stdin" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc ~docv:"FILE")

let xoff =
  let doc = "start at offset x" in
  Cmdliner.Arg.(value & pos 0 int 0 & info [] ~doc ~docv:"X_OFFSET")

let yoff =
  let doc = "start at offset y" in
  Cmdliner.Arg.(value & pos 1 int 0 & info [] ~doc ~docv:"Y_OFFSET")

let width =
  let doc = "width of the rectangle, in number of cross-stitches" in
  Cmdliner.Arg.(value & pos 2 int 1 & info [] ~doc ~docv:"WIDTH")

let height =
  let doc = "height of the rectangle, in number of cross-stitches" in
  Cmdliner.Arg.(value & pos 3 int 1 & info [] ~doc ~docv:"HEIGHT")

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

let info = Cmdliner.Term.info "piece" ~doc:"slice a piece out of an existing pattern"

let () = Cmdliner.(Term.exit @@ Term.eval (Term.(const piece $ xoff $ yoff $ width $ height $ file), info))
