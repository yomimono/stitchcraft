open Cmdliner

let files =
  let doc = "Images to concatenate together, leftmost first." in
  Arg.(non_empty & pos_all file [] & info [] ~doc)

let output =
  let doc = "output file. - for stdout, the default." in
  Arg.(value & opt string "-" & info ["output"; "o"] ~doc)

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go files output =
  let files = try
    List.map Yojson.Safe.from_file files |> List.rev
    with
    | _ -> failwith "couldn't read an input file"
  in
  let files = List.fold_left (fun acc yojson -> match acc, Stitchy.Types.(state_of_yojson yojson) with
      | Error e, _ | _, Error e -> Error e
      | Ok states, Ok state -> Ok (state::states)
    ) (Ok []) files
  in
  let bigpattern patterns =
    List.fold_left (fun bigpattern next -> match bigpattern with
        | None -> Some next
        | Some bigpattern -> Some (Compose_stitch.vcat bigpattern next))
      None patterns
  in
  match files with
  | Error _ -> failwith "oh no!!"
  | Ok patterns ->
    match bigpattern patterns with
    | None -> failwith "oh NOOOOOO!!!"
    | Some bigpattern ->
      Stitchy.Types.state_to_yojson bigpattern
      |> Yojson.Safe.to_file output
let vcat_t = Term.(const go $ files $ output)

let info = Term.info "vcat" ~doc:"vertically concatenate patterns"

let () = Term.exit @@ Term.eval (vcat_t, info)
