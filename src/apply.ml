let go fn (file : string) =
  fn (Util.pattern_or_die file) |> 
    Stitchy.Types.pattern_to_yojson |> Yojson.Safe.to_channel stdout
