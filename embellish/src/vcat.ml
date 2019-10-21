open Cmdliner

let left =
  let doc = "left image." in
  Arg.(value & pos 0 file "left.json" & info [] ~doc)

let right =
  let doc = "right image." in
  Arg.(value & pos 1 file "right.json" & info [] ~doc)

let output =
  let doc = "output file. - for stdout, the default." in
  Arg.(value & opt string "-" & info ["output"; "o"] ~doc)

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go left right output =
  let (left, right) = try
    Yojson.Safe.(from_file left, from_file right)
    with
    | _ -> failwith "couldn't read an input file"
  in
  match Stitchy.Types.(state_of_yojson left, state_of_yojson right) with
  | Error e, _ | _, Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
  | Ok left, Ok right ->
    Compose_stitch.vcat left right
    |> Stitchy.Types.state_to_yojson
    |> spoo output

let vcat_t = Term.(const go $ left $ right $ output)

let info = Term.info "vcat" ~doc:"vertically concatenate patterns"

let () = Term.exit @@ Term.eval (vcat_t, info)