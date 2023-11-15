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
