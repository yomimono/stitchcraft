let go border fill center output =
  let try_border s =
    Yojson.Safe.(from_file s) |>
    Stitchy.Types.border_of_yojson |> function
    | Error s -> failwith s
    | Ok p -> p
  in
  let center = Util.pattern_or_die center in
  let border = try_border border in
  let fill =
    match fill with
    | Some f -> Some (Util.pattern_or_die f)
    | None -> None
  in
  let embordered = Border.emborder ~center ~border ~fill in
  Util.output_or_die embordered output
