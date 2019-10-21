open Stitchy.Types

module Patterns = struct

  let blackstitch =
    { stitch = Full;
      thread = List.hd Stitchy.DMC.Thread.basic;
    }

  let white_bg max_x max_y : substrate =
    { max_x;
      max_y;
      background = 255, 255, 255;
      grid = Fourteen; }

  let smol =
    let substrate = white_bg 2 1 in
    let stitches = BlockMap.empty in
    let stitches = BlockMap.add (1, 1) blackstitch stitches in
    { substrate; stitches }

  let big =
    let substrate = white_bg 20 10 in
    let stitches = BlockMap.add (0, 0) blackstitch BlockMap.empty in
    let stitches = BlockMap.add (20, 10) blackstitch stitches in
    { substrate; stitches }

end

let substrate_size ?(name="substrate size") (x, y) pattern =
  Alcotest.(check @@ pair int int) name (x, y)
    (pattern.substrate.max_x, pattern.substrate.max_y)

let has_stitch ?(name="stitch present") (x, y) map =
  Alcotest.(check @@ bool) name true
    (Stitchy.Types.BlockMap.mem (x, y) map)

let n_stitches ?(name="number of stitches is right") n map =
  Alcotest.(check @@ int) name n (Stitchy.Types.BlockMap.cardinal map)

let check_vcat () =
  let vcat_smol = Compose_stitch.vcat Patterns.smol Patterns.smol in
  substrate_size (5,1) vcat_smol;
  has_stitch ~name:"left stitches present" (1, 1) vcat_smol.stitches;
  has_stitch ~name:"right stitches present" (4, 1) vcat_smol.stitches;
  n_stitches 2 vcat_smol.stitches

let check_hcat () =
  let hcat_smol = Compose_stitch.hcat Patterns.smol Patterns.smol in
  substrate_size (2, 3) hcat_smol;
  has_stitch ~name:"top stitches present" (1, 1) hcat_smol.stitches;
  has_stitch ~name:"bottom stitches present" (1, 3) hcat_smol.stitches;
  n_stitches 2 hcat_smol.stitches 

let bs_vcat () =
  let vcatted = Compose_stitch.vcat Patterns.smol Patterns.big in
  substrate_size (23, 10) vcatted;
  has_stitch (1, 6) vcatted.stitches;
  has_stitch (3, 0) vcatted.stitches;
  has_stitch (23, 10) vcatted.stitches;
  n_stitches 3 vcatted.stitches

let bs_hcat () =
  let hcatted = Compose_stitch.hcat Patterns.smol Patterns.big in
  substrate_size (20, 12) hcatted;
  has_stitch (10, 1) hcatted.stitches;
  has_stitch (0, 2) hcatted.stitches;
  has_stitch (20, 12) hcatted.stitches;
  n_stitches 3 hcatted.stitches

let () = Alcotest.run "concatenation" [
    ("no padding", [Alcotest.test_case "same dimensions vcat" `Quick check_vcat;
                    Alcotest.test_case "same dimensions hcat" `Quick check_hcat;
                   ]);
    ("padding and centering needed", [
        Alcotest.test_case "big/smol vcat" `Quick bs_vcat;
        Alcotest.test_case "big/smol hcat" `Quick bs_hcat;

      ]);
  ]
