let br = Borders.border_repetitions
let dim x_off y_off width height = Borders.({x_off; y_off; width; height;})

let pp_stitches fmt cs =
  let pp_list = Fmt.(list @@ pair ~sep:(Fmt.nop) int int) in
  pp_list fmt (Stitchy.Types.CoordinateSet.elements cs)

let pixel thread = Stitchy.Types.({
    substrate = {
      max_x = 0;
      max_y = 0;
      background = (0, 0, 0);
      grid = Fourteen;
    };
    layers = [{
        thread;
        stitch = Cross Full;
        stitches = CoordinateSet.of_list [(0, 0)];
      }];
    backstitch_layers = [];
  })

let close_borders () =
  (* if the side border is side 1 and there are no fenceposts,
   * the number of border repetitions should always be equal to the center *)
  let close_border center = br ~side:1 ~fencepost:0 ~center in
  Alcotest.(check int) "single-square border repetition should be equal to center width" 15 (close_border 15);
  Alcotest.(check int) "single-square border repetition should be equal to center width" 1 (close_border 1)

let fencepost_matters () =
  let center = 10 in
  let side = 2 in
  (* with no fencepost, we'd expect 2 *)
  Alcotest.(check int) "no fencepost evenly divisible" 5 (br ~side ~fencepost:0 ~center);
  (* with a size-1 fencepost, we'd expect 3, since fenceposts eat up 4 pixels *)
  Alcotest.(check int) "with fencepost evenly divisible" 3 (br ~side ~fencepost:1 ~center)

let needs_padding () =
  let center = 11 in
  let side = 2 in
  Alcotest.(check int) "no fencepost needs padding" 6 (br ~side ~fencepost:0 ~center);
  Alcotest.(check int) "with fencepost needs padding" 4 (br ~side ~fencepost:1 ~center)

let within () =
  Alcotest.(check bool) "0, 0 not within nonexistent w/h" false (Borders.within ~x:0 ~y:0 (dim 0 0 0 0));
  Alcotest.(check bool) "0, 0 within extant w/h" true (Borders.within ~x:0 ~y:0 (dim 0 0 1 1));
  Alcotest.(check bool) "1, 1 not within extant w/h" false (Borders.within ~x:1 ~y:1 (dim 0 0 1 1));
  Alcotest.(check bool) "0, 0 not within offset w/h" false (Borders.within ~x:0 ~y:0 (dim 10 10 1 1))

let pattern = Stitchy.Types.({
  substrate = {
    max_x = 2;
    max_y = 2;
    background = (0, 0, 0);
    grid = Fourteen;
  };
  layers = [{
      thread = List.hd Stitchy.DMC.Thread.basic;
      stitch = Cross Full;
      stitches = CoordinateSet.of_list [(0, 0); (1, 1);];
    }];
  backstitch_layers = [];
})

let tile () =
  let open Stitchy.Types in
  let tiled : pattern = Borders.tile pattern ~dimensions:(dim 0 0 10 10) ~mask_dimensions:[] in
  Alcotest.(check int) "tiled pattern is properly width-truncated" 9 tiled.substrate.max_x;
  Alcotest.(check int) "tiled pattern is properly height-truncated" 9 tiled.substrate.max_y;
  let expected_stitches = CoordinateSet.of_list [
      (0, 0); (3, 0); (6, 0); (9, 0);
      (1, 1); (4, 1); (7, 1);
      
      (0, 3); (3, 3); (6, 3); (9, 3);
      (1, 4); (4, 4); (7, 4);

      (0, 6); (3, 6); (6, 6); (9, 6);
      (1, 7); (4, 7); (7, 7);
      
      (0, 9); (3, 9); (6, 9); (9, 9);
    ] in
  let actual_stitches : CoordinateSet.t = (List.hd tiled.layers).stitches in
  Alcotest.(check (testable pp_stitches CoordinateSet.equal)) "tiled pattern has expected stitches" expected_stitches actual_stitches;
  ()

let masked_tile () =
  let open Stitchy.Types in
  let tiled = Borders.tile pattern ~dimensions:(dim 0 0 10 10) ~mask_dimensions:[(dim 2 2 5 5);
                                                                                 (dim 9 6 1 1);] in
  let expected_stitches = CoordinateSet.of_list [
      (0, 0); (3, 0); (6, 0); (9, 0);
      (1, 1); (4, 1); (7, 1);
      
      (0, 3);                 (9, 3);
      (1, 4);         (7, 4);

      (0, 6);                 
      (1, 7); (4, 7); (7, 7);
      
      (0, 9); (3, 9); (6, 9); (9, 9);
    ] in
  let actual_stitches : CoordinateSet.t = (List.hd tiled.layers).stitches in
  Alcotest.(check (testable pp_stitches CoordinateSet.equal)) "tiled pattern doesn't have stitches in masked area" expected_stitches actual_stitches;
  ()

let better_embellish_dimensions () =
  let corner = pixel (List.hd Stitchy.DMC.Thread.basic) in
  let center = {corner with layers = []} in
  let pattern = Borders.better_embellish ~fill:pattern ~corner ~top:corner ~center in
  (* since all elements have width and height 1, we expect not to need
   * any extra repetitions -- just enough to to surround the center *)
  let expected_stitches = Stitchy.Types.CoordinateSet.of_list [
      (0, 0); (1, 0); (2, 0);
      (0, 1);         (2, 1);
      (0, 2); (1, 2); (2, 2);
  ] in
  Alcotest.(check (testable pp_stitches Stitchy.Types.CoordinateSet.equal)) "better_embellish correctly handles tiny patterns" expected_stitches (List.hd pattern.layers).stitches;
  ()

let better_embellish_no_top () =
  let long_corner = Stitchy.Types.({
      substrate = {
        max_x = 2;
        max_y = 0;
        background = (0, 0, 0);
        grid = Fourteen;
      };
      layers = [{
          thread = List.hd Stitchy.DMC.Thread.basic;
          stitch = Cross Full;
          stitches = CoordinateSet.of_list [(1, 0)];
        }];
      backstitch_layers = [];
    })
  in
  let top = pixel (List.hd Stitchy.DMC.Thread.basic) in
  let center = {top with layers = []} in
  let pattern = Borders.better_embellish ~fill:center ~corner:long_corner ~center ~top in
  let expected_stitches = Stitchy.Types.CoordinateSet.of_list [ (1, 0); (3, 1); (0, 2); (2, 3) ] in
  Alcotest.(check (testable pp_stitches Stitchy.Types.CoordinateSet.equal)) "better_embellish correctly handles tiny patterns" expected_stitches (List.hd pattern.layers).stitches;

  ()

let () = Alcotest.(run "borders" @@ [
    ("compose", [
        ("close borders", `Quick, close_borders);
        ("fencepost matters", `Quick, fencepost_matters);
        ("with and without fencepost, not evenly divisible", `Quick, needs_padding);
      ]);
    ("within", [
        ("within", `Quick, within);
      ]);
    ("tile", [
        ("tile", `Quick, tile);
        ("tile with mask", `Quick, masked_tile);
      ]);
    ("better_embellish", [
        ("dimensions", `Quick, better_embellish_dimensions);
        ("no top repetitions", `Quick, better_embellish_no_top);
      ]);

  ])
