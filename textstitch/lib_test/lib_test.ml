module X = Stitchy.Types

let stitches = X.CoordinateSet.of_list [(1, 0); (0, 1); (2, 2);]
let test_glyph = { X.stitches;
                   backstitches = Stitchy.Types.SegmentSet.empty;
                   height = 3;
                   width = 4; }

let thread = match Stitchy.DMC.Thread.of_rgb (0, 0, 0) with
  | None -> failwith "thread lookup failure"
  | Some t -> t

let stitch = X.(Cross Full)

let substrate = {X.background = (255, 255, 255);
                 grid = X.Fourteen;
                 max_x = 3;
                 max_y = 2;
                }

let always_test_glyph _ = Some test_glyph
let never_a_glpyh _ = None

let one_glyph () =
  let min_width = 0 and min_height = 0 in
  let _, layer, _, width, height = Textstitch.render_phrase ~min_width ~min_height always_test_glyph thread [Uchar.of_char 'j'] 0 in
  let pattern = {backstitch_layers = []; X.substrate; layers = [layer]} in
  Format.printf "%a\n" Stitchy.Types.pp_pattern pattern;
  Alcotest.(check int "phrase with one glyph has the right number of stitches"
              (X.CoordinateSet.cardinal test_glyph.stitches)
              (X.CoordinateSet.cardinal layer.stitches));
  Alcotest.(check (pair int int) "phrase with one glyph is the same size as unconcatenated glyph"
              (test_glyph.width, test_glyph.height) (width, height));
  Alcotest.(check bool "phrase with one glyph has the same stitches as the glyph" true @@ X.CoordinateSet.equal layer.stitches test_glyph.stitches)

let () =
  Alcotest.run "say" [
    ("all the tests", [
        "one glyph", `Quick, one_glyph;
      ]
    )
  ]

