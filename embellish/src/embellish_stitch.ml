open Cmdliner

let corner =
  let doc = "Corner border image (oriented to upper-left corner)." in
  Arg.(value & opt file "corner.pattern" & info ["corner"] ~docv:"CORNER" ~doc)

let rotate_corners =
  let doc = "Rotate the corner image 90 degrees clockwise for each corner \
    going clockwise from the upper left." in
  Arg.(value & flag & info ["rotate"] ~docv:"ROTATE" ~doc)

let top =
  let doc = "top border pattern (oriented horizontally).  Will be repeated as necessary to fill the space needed, and transposed (not rotated) for the bottom border." in
  Arg.(value & opt file "top.pattern" & info ["top"] ~docv:"TOP" ~doc)

let fencepost =
  let doc = "Pattern to interpose between repetitions of the border, and between the first and last repetition and the corners." in
  Arg.(value & opt (some file) None & info ["fencepost"] ~docv:"FENCEPOST" ~doc)

let center =
  let doc = "Center pattern.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.pattern" & info ["center"] ~docv:"CENTER" ~doc)

let output =
  let doc = "Where to output the finished, embellished pattern. -, the default, is stdout." in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "embellish a pattern with corner and border images" in
  Term.info "embellish" ~doc ~exits:Term.default_exits

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go fencepost rotate_corners corner top center output =
  let (corner, top, center) = try
      Yojson.Safe.(from_file corner, from_file top, from_file center)
    with
    | _ -> failwith "couldn't read an input file"
  in
  let fencepost = match fencepost with
    | None -> Ok None
    | Some f ->
      try Yojson.Safe.from_file f |>
          Stitchy.Types.pattern_of_yojson |> function
          | Ok fencepost -> Ok (Some fencepost)
          | Error s -> Error s
      with _ -> Error "couldn't read fencepost pattern"
  in
  match Stitchy.Types.(fencepost,
                       pattern_of_yojson corner,
                       pattern_of_yojson top,
                       pattern_of_yojson center) with
  | Ok fencepost, Ok corner, Ok top, Ok center ->
    Borders.embellish ~fencepost ~rotate_corners ~center ~corner ~top
    |> Stitchy.Types.pattern_to_yojson
    |> spoo output
  | _, _, _, _ -> failwith (Printf.sprintf "failed to parse input json")

let compose_t = Term.(const go $ fencepost $ rotate_corners $ corner $ top $ center $ output)

let () = Term.exit @@ Term.eval (compose_t, info)
