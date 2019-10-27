let stdout_or_file json output =
  try
    if 0 = String.compare output "-" then
      Ok (Yojson.Safe.to_channel stdout json)
    else Ok (Yojson.Safe.to_file output json)
  with
  | _ -> Error output

let stdin_or_file input =
  try
    if 0 = String.compare input "-" then
      Ok Yojson.Safe.(from_channel stdin)
    else Ok Yojson.Safe.(from_file input)
  with
  | _ -> Error input
