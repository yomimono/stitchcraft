module X = Stitchy.Types

(* we shouldn't generate any layers we weren't given *)
let empty_layers () =
  let assembled_empty = Assemble.stitch (0, 0, 0) 0 0 X.Fourteen [] in
  Alcotest.(check int "assembling empty layers should give an empty pattern" 0 @@ List.length assembled_empty.layers)

let disjoint_layers () =
  let thread_1 = List.hd Stitchy.DMC.Thread.basic
  and thread_2 = List.nth Stitchy.DMC.Thread.basic 1 in
  let layer_1 = { X.thread = thread_1; stitch = Cross Full; stitches = X.CoordinateSet.singleton (1, 1) }
  and layer_2 = { X.thread = thread_2; stitch = Cross Full; stitches = X.CoordinateSet.singleton (0, 0) }
  in
  let assembled = Assemble.stitch (0, 0, 0) 2 2 Stitchy.Types.Fourteen [layer_1; layer_2] in
  Alcotest.(check int "both layers are present when we assemble layers of different color" 2 @@ List.length assembled.layers)

let mergeable_layers () =
  let thread = List.hd Stitchy.DMC.Thread.basic in
  let layer_1 = {X.thread = thread; stitch = Cross Full; stitches = X.CoordinateSet.singleton (0, 0) }
  and layer_2 = {X.thread = thread; stitch = Cross Full; stitches = X.CoordinateSet.singleton (1, 1) }
  in
  let assembled = Assemble.stitch (0, 0, 0) 2 2 Stitchy.Types.Fourteen [layer_1; layer_2] in
  Alcotest.(check int "assembling two layers with the same thread and stitch gives 1 layer" 1 @@ List.length assembled.layers);
  let only_layer = List.hd assembled.layers in
  Alcotest.(check int "merged layer has both stitches" 2 (X.CoordinateSet.cardinal only_layer.stitches))

let () =
  Alcotest.run "assemble" [
    ("all the tests", [
        ("empty list of layers", `Quick, empty_layers);
        ("disjoint layers are not merged", `Quick, disjoint_layers);
        ("layers that can be merged, are", `Quick, mergeable_layers);
      ]
    )
  ]

