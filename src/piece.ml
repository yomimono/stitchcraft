let piece x_off y_off width height file =
  let pattern = Util.pattern_or_die file in
  let submap = Stitchy.Types.submap ~x_off ~y_off ~width ~height pattern in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson submap
