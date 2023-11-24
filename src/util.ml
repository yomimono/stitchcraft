let stdin_or_file input =
  try
    if 0 = String.compare input "-" then
      Ok Yojson.Safe.(from_channel stdin)
    else Ok Yojson.Safe.(from_file input)
  with
  | _ -> Error input

let pattern_of_input input =
  match stdin_or_file input with
  | Ok json -> Stitchy.Types.pattern_of_yojson json
  | Error _ as e -> e

let pattern_or_die input =
  match stdin_or_file input with
  | Error s -> Format.eprintf "error reading input %s: %s\n%!" input s; exit 1
  | Ok json ->
    match Stitchy.Types.pattern_of_yojson json with
    | Error s -> Format.eprintf "error parsing pattern %s: %s\n%!" input s; exit 1
    | Ok p -> p

let stdout_or_file json output =
  try
    if 0 = String.compare output "-" then
      Ok (Yojson.Safe.to_channel stdout json)
    else Ok (Yojson.Safe.to_file output json)
  with
  | _ -> Error output

let pattern_to_output pattern output =
  stdout_or_file (Stitchy.Types.pattern_to_yojson pattern) output

let output_or_die pattern output =
  match pattern_to_output pattern output with
  | Ok () -> ()
  | Error e ->
    Format.eprintf "error outputting result: %s\n%!" e; exit 1
