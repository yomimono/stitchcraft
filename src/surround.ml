let go border center output =
  let try_border s =
    Yojson.Safe.(from_file s) |>
    Stitchy.Types.border_of_yojson |> function
    | Error s -> failwith s
    | Ok p -> p
  in
  match Util.pattern_of_input center with
  | Error s -> failwith s
  | Ok center ->
    let border = try_border border in
    let embordered = Border.emborder ~center ~border ~fill:None in
    match Util.pattern_to_output embordered output with
    | Ok () -> ()
    | Error s -> failwith s
