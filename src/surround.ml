let go border center output =
  let try_border s =
    Yojson.Safe.(from_file s) |>
    Stitchy.Types.border_of_yojson |> function
    | Error s -> failwith s
    | Ok p -> p
  in
  let center = Util.pattern_or_die center in
  let border = try_border border in
  let embordered = Border.emborder ~center ~border ~fill:None in
  Util.output_or_die embordered output
