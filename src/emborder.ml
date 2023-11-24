let corner_no_transformation_doc =
  {|
    A corner image given by itself with no transformation
    will be repeated without change in its given orientation, e.g. for a
    4x4 corner image tiled 4 times to border a center image,
    the resulting tiling will look like this:

    +----+----+----+----+
    |x  o|x  o|x  o|x  o|
    |    |    |    |    |
    |x  x|x  x|x  x|x  x|
    | xx | xx | xx | xx |
    +----+----+----+----+
    |x  o|         |x  o|
    |    |         |    |
    |x  x|         |x  x|
    | xx |  center | xx |
    +----+         +----+
    |x  o|  image! |x  o|
    |    |         |    |
    |x  x|         |x  x|
    | xx |         | xx |
    +----+----+----+----+
    |x  o|x  o|x  o|x  o|
    |    |    |    |    |
    |x  x|x  x|x  x|x  x|
    | xx | xx | xx | xx |
    +----+----+----+----+
    |}

let corner_rotate_transformation_doc = {|
    For a corner image with the "rotate" transformation, the image will be rotated 90 degrees
    around each corner to form the border on each side.
    The initial corner image forms the upper-left-hand corner.
    For example, a 6x4 corner image,
    which is repeated once and then rotated to form the corner:

    +------+------+----+
    | xx xx| xx xx| xx |
    |x  x  |x  x  |x  x|
    |x  x  |x  x  |x  x|
    | xx xx| xx xx| xx |
    +------+------+x  x|
    |x  x|        |x  x|
    |x  x|        +----+
    | xx |   hi   | xx |
    |x  x|        |x  x|
    |x  x| parent |x  x|
    | xx |        | xx |
    +----+   :)   |x  x|
    |x  x|        |x  x|
    |x  x+--------+----+
    | xx |xx xx |xx xx |
    |x  x|  x  x|  x  x|
    |x  x|  x  x|  x  x|
    | xx |xx xx |xx xx |
    +----+-------------+
|}
let corner_flip_transformation_doc = {|
  For a corner image with the "flip", the image will be repeated horizontally
  and flipped horizontally whenever a repetition has passed the x-axis centerpoint.
  To form the bottom edge, the same tactic is used but flipped vertically.
  To form the left and right sides, the image is rotated 90 degrees clockwise (for the right side)
  and 270 degrees clockwise (for the left side); this is then repeated until it
  crosses the y-axis centerpoint, at which point it's flipped for further repetition.
  For example, a 6x4 corner image:

  +------+------+------+------+
  |   x  |   x  |  x   |  x   |
  |  x  x|  x  x|x  x  |x  x  |
  | x  x | x  x | x  x | x  x |
  |x  x  |x  x  |  x  x|  x  x|
  +------+------+------+------+
  | x  |                 |  x |
  |  x |                 | x  |
  |x  x|                 |x  x|
  | x  |                 |  x |
  |  x |                 | x  |
  |   x|                 |x   |
  +----+                 +----+
  |   x|                 |x   |
  |  x |                 | x  |
  | x  |                 |  x |
  |x  x|                 |x  x|
  |  x |                 | x  |
  | x  |                 |  x |
  +----+-----------------+----+
  |x  x  |x  x  |  x  x|  x  x|
  | x  x | x  x | x  x | x  x |
  |  x  x|  x  x|x  x  |x  x  |
  |   x  |   x  |  x   |  x   |
  +------+------+------+------+
    |}

let with_side = {|
  Supplying a 'side' pattern replaces repetitions of the corner pattern, if
  they would exist, with 'side'.  For example,
  consider a 6x6 corner pattern with a 4x4 side pattern, using the 'rotate'
  transformation for both:

  +------+         +------+
  |xxxxx +----+----+ xxxxx|
  |xyyyyx|   x|   x|xyyyyx}
  |xy   y|x xy|x xy|y   yx|
  |xy    |yxy |yxy |    yx|
  |xy    | y  | y  |    yx|
  | xy   +----+----+   yx |
  ++----++         +------+
   |xy  |           | yx |
   | xy |           |yx  |
   |  xy|  a very   | yx |
   | xy |           |  yx|
   +----+           +----+
   |xy  |  pretty   | yx |
   | xy |           |yx  |
   |  xy|           | yx |
   | xy |           |  yx|
  +------+         +------+
  | xy   +----+----+   yx |
  |xy    |  y |  y |    yx}
  |xy    | yxy| yxy|    yx|
  |xy   y|yx x|yx x|y   yx|
  |xyyyyx|x   |x   |xyyyyx|
  |xxxxx +----+----+ xxxxx|
  +------+         +------+
  |}

let with_fencepost = {|
  Supplying a "fencepost" pattern gives a separate pattern which
  will be inserted between the corners (and sides, if supplied).
  It's useful for repeating elements which use a short "joining" segment.

  For example, here's a border with a 4x4 corner and a 4x4 side,
  with a 1x4 fencepost, with all patterns using the rotation transform.

  +----+-+----+-+----+-+----+
  |    |x|    |x|    |x|    |
  |   x| |x  x| |x  x| |x   |
  |  x | | xx | | xx | | x  |
  | x  | |    | |    | |  x |
  +----+-+----+-+----+-+----+
  |x   |               |   x|
  +----+               +----+
  | x  |               |  x |
  |  x |   GET HYPE    | x  |
  |  x |               | x  |
  | x  |  for stitch   |  x |
  +----+               +----+
  |x   |               |   x|
  +----+-+----+-+----+-+----+
  | x  | |    | |    | |  x |
  |  x | | xx | | xx | | x  |
  |   x| |x  x| |x  x| |x   |
  |    |x|    |x|    |x|    |
  +----+-+----+-+----+-+----+
  |}

let try_pattern s =
  Yojson.Safe.(from_file s) |>
  Stitchy.Types.pattern_of_yojson |> function
  | Error s -> failwith s
  | Ok p -> p

let entypify file transformation =
  match file with
  | None -> None
  | Some f -> Some Stitchy.Types.({
      transformation;
      pattern = try_pattern f;
    })


let go corner side fencepost corner_xform side_transform fencepost_transform output =
  match Util.stdin_or_file corner with
  | Error s -> failwith s
  | Ok json ->
    match Stitchy.Types.pattern_of_yojson json with
    | Error s -> failwith s
    | Ok pattern ->
      let open Stitchy.Types in
      let corner = { pattern; transformation = corner_xform } in
      let border = { corner;
                     side = entypify side side_transform;
                     fencepost = entypify fencepost fencepost_transform;
                   }
      in
      match Util.stdout_or_file (border_to_yojson border) output with
      | Error s -> failwith s
      | Ok () -> ()
