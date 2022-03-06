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
  let files = List.fold_left (fun acc yojson -> match acc, Stitchy.Types.(pattern_of_yojson yojson) with
      | Error e, _ | _, Error e -> Error e
      | Ok patterns, Ok pattern -> Ok (pattern::patterns)
    ) (Ok []) files
  in
  match files with
  | Error _ -> failwith "oh no!!"
  | Ok patterns ->
    match Stitchy.Operations.(perform Vcat patterns) with
    | Error (`Msg s) -> Format.eprintf "error performing concatenation: %s\n%!" s;
      exit 1
    | Ok (bigpattern::_) ->
      Stitchy.Types.pattern_to_yojson bigpattern
      |> Yojson.Safe.to_file output
    | Ok _ ->
      Format.eprintf "multiple patterns came back from hcat";
      exit 1

let vcat_t = Term.(const go $ files $ output)

let info = Cmd.info "vcat" ~doc:"vertically concatenate patterns"

let () = exit @@ Cmd.eval @@ Cmd.v info vcat_t
