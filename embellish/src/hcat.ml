open Cmdliner

let files =
  let doc = "Image(s) to concatenate, in order with topmost appearing first." in
  Arg.(non_empty & pos_all file [] & info [] ~doc)

let output =
  let doc = "output file" in
  Arg.(value & opt string "tall.json" & info ["output"; "o"] ~doc)

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
  let bigpattern patterns =
    List.fold_left (fun bigpattern next -> match bigpattern with
        | None -> Some next
        | Some bigpattern -> Some (Stitchy.Operations.hcat bigpattern next))
      None patterns
  in
  match files with
  | Error _ -> failwith "oh no!!"
  | Ok patterns ->
    match bigpattern patterns with
    | None -> failwith "oh NOOOOOO!!!"
    | Some bigpattern ->
      Stitchy.Types.pattern_to_yojson bigpattern
      |> Yojson.Safe.to_file output

let hcat_t = Term.(const go $ files $ output)

let info = Term.info "hcat" ~doc:"horizontally concatenate patterns"

let () = Term.exit @@ Term.eval (hcat_t, info)
