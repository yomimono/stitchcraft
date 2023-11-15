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
    match Stitchy.Operations.(perform Hcat patterns) with
    | Error (`Msg s) ->
      Format.eprintf "error concatenating: %s\n!" s;
      exit 1
    | Ok (bigpattern::_) -> begin
      Stitchy.Types.pattern_to_yojson bigpattern
      |> Yojson.Safe.to_file output
    end
    | Ok _ ->
      Format.eprintf "too many elements returned";
      exit 1
