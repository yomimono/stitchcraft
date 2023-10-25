let go border center output =
  let try_pattern s =
    Yojson.Safe.(from_file s) |>
    Stitchy.Types.pattern_of_yojson |> function
    | Error s -> failwith s
    | Ok p -> p
  in
  let try_border s =
    Yojson.Safe.(from_file s) |>
    Stitchy.Types.border_of_yojson |> function
    | Error s -> failwith s
    | Ok p -> p
  in
  let border = try_border border and center = try_pattern center in
  let embordered = Border.emborder ~center ~border ~fill:None in
  match Stitchy.Files.stdout_or_file (Stitchy.Types.pattern_to_yojson embordered) output with
  | Ok () -> ()
  | Error s -> failwith s
