open Cmdliner

let top =
  let doc = "Top image." in
  Arg.(value & pos 0 file "top.json" & info [] ~doc)

let bottom =
  let doc = "Bottom image." in
  Arg.(value & pos 1 file "bottom.json" & info [] ~doc)

let output =
  let doc = "output file" in
  Arg.(value & opt string "tall.json" & info ["output"; "o"] ~doc)

let go top bottom output =
  let (top, bottom) = try
    Yojson.Safe.(from_file top, from_file bottom)
    with
    | _ -> failwith "couldn't read an input file"
  in
  match Stitchy.Types.(state_of_yojson top, state_of_yojson bottom) with
  | Error e, _ | _, Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
  | Ok top, Ok bottom ->
    Compose_stitch.hcat top bottom
    |> Stitchy.Types.state_to_yojson
    |> Yojson.Safe.to_file output

let hcat_t = Term.(const go $ top $ bottom $ output)

let info = Term.info "hcat" ~doc:"horizontally concatenate patterns"

let () = Term.exit @@ Term.eval (hcat_t, info)
