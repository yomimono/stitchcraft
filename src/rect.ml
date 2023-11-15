let rect width height bg thread gridsize x y =
  let substrate = Primitives.empty bg gridsize ~width:(x + width) ~height:(y + height) in
  let pattern = Primitives.rect substrate.Stitchy.Types.substrate thread x y width height in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson pattern
