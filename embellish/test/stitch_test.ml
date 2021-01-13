open Stitchy.Types

module Patterns = struct

  let blackstitch =
    { stitch = Cross Full;
      thread = List.hd Stitchy.DMC.Thread.basic;
      stitches = []
    }

  let white_bg max_x max_y : substrate =
    { max_x;
      max_y;
      background = 255, 255, 255;
      grid = Fourteen; }

  let smol =
    let substrate = white_bg 2 1 in
    let layer = {blackstitch with stitches = [(1, 1)]} in
    { substrate; layers = layer::[]}

  let big =
    let substrate = white_bg 20 10 in
    let layer = {blackstitch with stitches = [(0, 0); (20, 10)]} in
    { substrate; layers = layer::[]}

end

let substrate_size ?(name="substrate size") (x, y) pattern =
  Alcotest.(check @@ pair int int) name (x, y)
    (pattern.substrate.max_x, pattern.substrate.max_y)

let n_stitches ?(name="number of stitches is right") n pattern =
  let stitches = List.fold_left (fun acc (layer : layer) -> acc + List.length layer.stitches) 0 pattern.layers in
  Alcotest.(check @@ int) name n stitches

let has_stitch ?(name="stitch present") pattern (x, y) =
  Alcotest.(check @@ int) name 1
    (Stitchy.Types.stitches_at pattern (x, y) |> List.length)

let check_vcat () =
  let vcat_smol = Compose_stitch.vcat Patterns.smol Patterns.smol in
  substrate_size (5,1) vcat_smol;
  has_stitch ~name:"left stitches present" vcat_smol (1, 1);
  has_stitch ~name:"right stitches present" vcat_smol (4, 1);
  n_stitches 2 vcat_smol

let check_hcat () =
  let hcat_smol = Compose_stitch.hcat Patterns.smol Patterns.smol in
  substrate_size (2, 3) hcat_smol;
  has_stitch ~name:"top stitches present" hcat_smol (1, 1);
  has_stitch ~name:"bottom stitches present" hcat_smol (1, 3);
  n_stitches 2 hcat_smol

let bs_vcat () =
  let vcatted = Compose_stitch.vcat Patterns.smol Patterns.big in
  substrate_size (23, 10) vcatted;
  List.iter (has_stitch vcatted) [(1,6); (3,0); (23,10)];
  n_stitches 3 vcatted

let bs_hcat () =
  let hcatted = Compose_stitch.hcat Patterns.smol Patterns.big in
  substrate_size (20, 12) hcatted;
  List.iter (has_stitch hcatted) [(10, 1); (0, 2); (20, 12)];
  n_stitches 3 hcatted

let () = Alcotest.run "concatenation" [
    ("no padding", [Alcotest.test_case "same dimensions vcat" `Quick check_vcat;
                    Alcotest.test_case "same dimensions hcat" `Quick check_hcat;
                   ]);
    ("padding and centering needed", [
        Alcotest.test_case "big/smol vcat" `Quick bs_vcat;
        Alcotest.test_case "big/smol hcat" `Quick bs_hcat;

      ]);
  ]
