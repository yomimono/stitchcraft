open Cmdliner

let corner =
  let doc = "Corner border image (oriented to upper-left corner)." in
  Arg.(value & opt file "corner.pattern" & info ["corner"] ~docv:"CORNER" ~doc)

let rotate_corners =
  let doc = "Rotate the corner image 90 degrees clockwise for each corner \
    going clockwise from the upper left." in
  Arg.(value & flag & info ["rotate"] ~docv:"ROTATE" ~doc)

let top =
  let doc = "top border image (oriented horizontally).  Will be flipped (not rotated) for the bottom border." in
  Arg.(value & opt file "top.pattern" & info ["top"] ~docv:"TOP" ~doc)

let side =
  let doc = "left side border image.  Will be flipped (not rotated) for the right border." in
  Arg.(value & opt file "side.pattern" & info ["side"] ~docv:"LEFT" ~doc)

let center =
  let doc = "Center image.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.pattern" & info ["center"] ~docv:"CENTER" ~doc)

let output =
  let doc = "Where to output the finished, embellished image. -, the default, is stdout." in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "embellish an image with corner and border images" in
  Term.info "embellish" ~doc ~exits:Term.default_exits

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go rotate_corners corner top side center output =
  let (corner, top, side, center) = try
      Yojson.Safe.(from_file corner, from_file top, from_file side, from_file center)
    with
    | _ -> failwith "couldn't read an input file"
  in
  match Stitchy.Types.(pattern_of_yojson corner,
                       pattern_of_yojson top,
                       pattern_of_yojson side,
                       pattern_of_yojson center) with
  | Ok corner, Ok top, Ok side, Ok center ->
    Borders.embellish ~rotate_corners ~center ~corner ~top ~side
    |> Stitchy.Types.pattern_to_yojson
    |> spoo output
  | _, _, _, _ -> failwith (Printf.sprintf "failed to parse input json")

let compose_t = Term.(const go $ rotate_corners $ corner $ top $ side $ center $ output)

let () = Term.exit @@ Term.eval (compose_t, info)
