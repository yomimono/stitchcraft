let bits_set n =
  let bit_set b =
    (n land b) = b
  in
  let l = ref [] in
  if bit_set 0b10000000 then l := 0::!l;
  if bit_set 0b01000000 then l := 1::!l;
  if bit_set 0b00100000 then l := 2::!l;
  if bit_set 0b00010000 then l := 3::!l;
  if bit_set 0b00001000 then l := 4::!l;
  if bit_set 0b00000100 then l := 5::!l;
  if bit_set 0b00000010 then l := 6::!l;
  if bit_set 0b00000001 then l := 7::!l;
  !l

let rec next_char ~max_x (x, stitches) =
  try
    let c = input_char Stdlib.stdin in
    let bits = bits_set (Char.code c) in
    let stitches =
      List.fold_left (fun stitches b ->
          let overall_x = x + b in
          (* don't add any stitches off the end of the substrate *)
          if overall_x > max_x then stitches else
            Stitchy.Types.CoordinateSet.add ((x + b), 0) stitches
        ) stitches bits
    in
    next_char ~max_x (x + 8, stitches)
  with
  | End_of_file -> (x, stitches)

let hex width background grid thread =
  (* unlike other invocations, we make the user give us a width,
   * and the height is always 1,
   * so we can just set the substrate right away *)
  let max_x = width - 1 in
  let substrate : Stitchy.Types.substrate = {max_x;
                                             max_y = 0; background; grid} in
  let stitches = Stitchy.Types.CoordinateSet.empty in
  let stitches = snd @@ next_char ~max_x (0, stitches) in
  let pattern : Stitchy.Types.pattern =
    {substrate;
     layers = [{thread;
                stitch = Cross Full;
                stitches;
               }];
     backstitch_layers = [];
    }
  in
  Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_channel stdout
