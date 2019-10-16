open Cmdliner

let corner =
  let doc = "Corner border image (oriented to upper-left corner).  Will be flipped (not rotated) as appropriate for other corners." in
  Arg.(value & opt file "corner.png" & info ["corner"] ~docv:"CORNER" ~doc)

let side =
  let doc = "Side border image (oriented horizontally).  Will be flipped (not rotated) as appropriate for other sides." in
  Arg.(value & opt file "side.png" & info ["side"] ~docv:"SIDE" ~doc)

let center =
  let doc = "Center image.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.png" & info ["center"] ~docv:"CENTER" ~doc)

let output =
  let doc = "Path at which to output the finished, embellished image." in
  Arg.(value & opt string "embellished.json" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "embellish an image with corner and border images" in
  Term.info "embellish" ~doc ~exits:Term.default_exits

let go corner side center output =
  let (corner, side, center) = try
      Yojson.Safe.(from_file corner, from_file side, from_file center)
    with
    | _ -> failwith "couldn't read an input file"
  in
  match Stitchy.Types.(state_of_yojson corner, state_of_yojson side, state_of_yojson center) with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
  | Ok corner, Ok side, Ok center ->
    Compose_stitch.embellish ~center ~corner ~side
    |> Stitchy.Types.state_to_yojson
    |> Yojson.Safe.to_file output

let compose_t = Term.(const go $ corner $ side $ center $ output)

let () = Term.exit @@ Term.eval (compose_t, info)
