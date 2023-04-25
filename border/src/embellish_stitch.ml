open Cmdliner

(* the end result, in crude ASCII art, of an invocation with corner, top, fencepost
 * with rotate_corners set.
 *
 * if corner is 4x4, fencepost ("fp") is 2x4, and top is 6x4:

+----+--+------+--+------+--+----+
|cor-|  |      |  |      |  |cor-|
|ner |fp| top  |fp| top  |fp|ner |
|0 ro|  |      |  |      |  |90d |
|tate|  |      |  |      |  | CW |
+----+--+------+--+------+--+----+
|fp90|                      | fp |
| CCW|                      |90CW|
+----+                      +----+
|    |                      |    |
| top|                      | top|
|90d |                      |90d |
| ccw|                      |  cw|
|    |                      |    |
|    |                      |    |
+----+                      +----+
|fp90|                      | fp |
| CCW|                      |90CW|
+----+--+------+--+------+--+----+
|cor-|fp|      |fp|      |fp|cor-|
|ner |2x| top  |2x| top  |2x|ner |
|90  |90| 180  |90| 180  |90|180d|
| CCW|cw| cw   |cw| cw   |cw| CW |
+----+--+------+--+------+--+----+
*)

let corner =
  let doc = "Corner border image (oriented to upper-left corner)." in
  Arg.(value & opt file "corner.pattern" & info ["corner"] ~docv:"CORNER" ~doc)

let rotate_corners =
  let doc = "Rotate the corner image 90 degrees clockwise for each corner \
    going clockwise from the upper left." in
  Arg.(value & flag & info ["rotate"] ~docv:"ROTATE" ~doc)

let top =
  let doc = "top border pattern (oriented horizontally).  Will be repeated as necessary to fill the space needed, and rotated to form the left, bottom, and right borders." in
  Arg.(value & opt file "top.pattern" & info ["top"] ~docv:"TOP" ~doc)

let fencepost =
  let doc = "Pattern to interpose between repetitions of the border, and between the first and last repetition and the corners." in
  Arg.(value & opt (some file) None & info ["fencepost"] ~docv:"FENCEPOST" ~doc)

let center =
  let doc = "Center pattern.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.pattern" & info ["center"] ~docv:"CENTER" ~doc)

let min_width =
  let doc = "minimum width for the final pattern" in
  Arg.(value & opt int 1 & info ["min_width"] ~docv:"MIN_WIDTH" ~doc)

let output =
  let doc = "Where to output the finished, embellished pattern. -, the default, is stdout." in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "embellish a pattern with corner and border images" in
  Cmd.info "embellish" ~doc

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go fencepost rotate_corners corner top center min_width output =
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
    Border.square_corner_embellish ~min_width ~fencepost ~rotate_corners ~center ~corner ~top
    |> Stitchy.Types.pattern_to_yojson
    |> spoo output
  | _, _, _, _ -> failwith (Printf.sprintf "failed to parse input json")

let compose_t = Term.(const go $ fencepost $ rotate_corners $ corner $ top $ center $ min_width $ output)

let () = exit @@ Cmd.eval @@ Cmd.v info compose_t
