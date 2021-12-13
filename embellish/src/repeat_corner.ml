(* "repeating corner" borders operate on:
 *
 * a "corner (maybe plus a bit)" image, which always gets applied 4 times.
 * it's copied verbatim in the upper-left, then rotated 90 degrees
 * clockwise for the upper-right, 180 for the lower-right, and 90
 * counterclockwise for the lower left.
 *
 * a repeating segment, which fills in any gaps between the corners.
 * a whole number of repeating segments will be inserted to make the
 * border's length equal to or longer than the center image.
 *
 * a center image, which is unchanged other than being placed centrally
 * within the generated border.
 *
 * The end result can be described in crude ASCII art:
  
    01234567 89abcde 0123456789abcdef 01234567
   +----------------+----------------+--------+
  0|                |                |        |
  1|                |                |        |
  2|                |                |        |
  3|     c2         |    b2          |   c1   |
  4|  no rotation   |   no rotation  |        |
  5|                |                |clock-  |
  6|                |                |  wise  |
  7|                |                |  90    |
   +----------------+----------------| degree | 
  0|        |    p a d d i n g       |rotation|8
  1|        |                        |        |9
  2|        |                        |        |a
  3|        |                        |        |b
  4|  b4    |                       p|        |c
  5|counter |                        |        |d
  6|clock-  |                       a|        |e
   |  wise  |                        +--------+
  7| 90     |p                      d|        |0
  8|degree  |                        |        |1
  9|rotation|a                      d|   b1   |2
  a|        |                        |clock-  |3
  b|        |d                      i|  wise  |4
  c|        |         center         |  90    |5
  d|        |d        image         n| degree |6
  e|        |                        |rotation|7
  f|        |i                      g|        |8
   +--------+                        |        |
  0|        |i                       |        |9
  1|        |                        |        |a
  2|        |n                       |        |b
  3|  c3    |                        |        |c
  4| counter|g                       |        |d
  5|clock-  |                        |        |e
  6|  wise  |      p a d d i n g     |        |f
   | 90     |----------------+-------+--------+
  7| degree |                |                |0
  8|rotation|                |                |1
  9|        |     b2         |      c2        |2
  a|        |  180 degree    |  180 degree    |3
  b|        |   rotation     |    rotation    |4
  c|        |                |                |5
  d|        |                |                |6
  e|        |                |                |7
   +--------+----------------+----------------+
    76543210 fedcba9876543210 edcba98 76543210

*)
open Cmdliner

let corner =
  let doc = "Corner border image (oriented to upper-left corner). \
            This image will be rotated and repeated for each corner." in
  Arg.(value & opt file "corner.pattern" & info ["corner"] ~docv:"CORNER" ~doc)

let border =
  let doc = "border image (oriented as it will appear on top border). \
    This image will be rotated and repeated as necessary to construct a
    full border around the center image, linking the rotated corner images." in
  Arg.(value & opt file "border.pattern" & info ["border"] ~docv:"BORDER" ~doc)

let center =
  let doc = "Center image.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.pattern" & info ["center"] ~docv:"CENTER" ~doc)

let output =
  let doc = "Where to output the finished, embellished image. -, the default, is stdout." in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let fill =
  let doc = "a fill for any space generated to make borders fit properly. If none desired, try `empty 1 1`" in
  Arg.(value & opt file "fill.pattern" & info ["fill"] ~docv:"FILL" ~doc)

let info =
  let doc = "embellish an image with corner and border images" in
  Term.info "embellish" ~doc ~exits:Term.default_exits

let spoo output json =
  if 0 = String.compare output "-" then Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let go corner border center fill output =
  let (corner, border, center, fill) = try
      Yojson.Safe.(from_file corner, from_file border, from_file center, from_file fill)
    with
    | _ -> failwith "couldn't read an input file"
  in
  match Stitchy.Types.(pattern_of_yojson corner,
                       pattern_of_yojson border,
                       pattern_of_yojson center,
                       pattern_of_yojson fill) with
  | Ok corner, Ok top, Ok center, Ok fill -> begin
    match Stitchy.Types.(corner.substrate.max_y, top.substrate.max_y) with
    | a, b when compare a b <> 0 ->
      failwith (Printf.sprintf "Corner and border images must have the height")
    | _, _ ->
      Borders.better_embellish ~fill ~center ~corner ~top
      |> Stitchy.Types.pattern_to_yojson
      |> spoo output
    end
  | _ -> failwith (Printf.sprintf "failed to parse input json")

let compose_t = Term.(const go $ corner $ border $ center $ fill $ output)

let () = Term.exit @@ Term.eval (compose_t, info)
