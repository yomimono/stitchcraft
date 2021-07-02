open Stitchy.Types

(* TODO: a few tests here just exercise merge_threads, and should be moved over to the stitchy library *)

module Patterns = struct

  let blackstitch =
    { stitch = Cross Full;
      thread = List.hd Stitchy.DMC.Thread.basic;
      stitches = CoordinateSet.empty;
    }

  let white_bg max_x max_y : substrate =
    { max_x;
      max_y;
      background = 255, 255, 255;
      grid = Fourteen; }

  let smol =
    let substrate = white_bg 2 1 in
    let layer = {blackstitch with stitches = CoordinateSet.singleton (1, 1)} in
    { substrate; backstitch_layers = []; layers = layer::[]}

  let big =
    let substrate = white_bg 20 10 in
    let layer = {blackstitch with stitches =
                                    CoordinateSet.of_list [(0, 0); (20, 10)]} in
    { substrate; layers = layer::[]; backstitch_layers = []; }

end

let substrate_size ?(name="substrate size") (x, y) pattern =
  Alcotest.(check @@ pair int int) name (x, y)
    (pattern.substrate.max_x, pattern.substrate.max_y)

let stitches_all_in_substrate (pattern : Stitchy.Types.pattern) =
        let stitch_in_substrate (x, y) = pattern.substrate.max_x >= x
        && pattern.substrate.max_y >= y
        && x >= 0 && y >= 0 in
        let check_stitches_in_layer (layer : Stitchy.Types.layer) =
        CoordinateSet.iter (fun stitch -> Alcotest.(check bool) "stitch is within substrate" (stitch_in_substrate stitch) true) layer.stitches
        in
        List.iter check_stitches_in_layer pattern.layers

let n_stitches ?(name="number of stitches is right") n pattern =
  let stitches = List.fold_left (fun acc (layer : layer) -> acc + CoordinateSet.cardinal layer.stitches) 0 pattern.layers in
  Alcotest.(check @@ int) name n stitches

let has_stitch ?(name="stitch present") pattern (x, y) =
  Alcotest.(check @@ int) name 1
    (Stitchy.Types.stitches_at pattern (x, y) |> List.length)

let no_padding ~f () =
  match f Patterns.smol Patterns.smol with
  | `None, first,second  -> Alcotest.(check @@ pair int int) "vpadding for 2x same should be 0" (0, 0) (first, second)
  | _ -> Alcotest.fail "vpadding for 2x same shouldn't return that someone needs to pad"

let no_vpadding = no_padding ~f:Compose_stitch.vpadding
let no_hpadding = no_padding ~f:Compose_stitch.hpadding

let first_needs_padding_hcat () =
        match Compose_stitch.hpadding Patterns.smol Patterns.big with
        | `First, left, right -> Alcotest.(check @@ pair int int) "correct hcat padding for a small top item" (9, 9) (left, right)
        | _ -> Alcotest.fail "hcat padding for smol, large should pad smol"

let second_needs_padding_hcat () =
        match Compose_stitch.hpadding Patterns.big Patterns.smol with
        | `Second, left, right -> Alcotest.(check @@ pair int int) "correct hcat padding for a small bottom item" (9, 9) (left, right)
        | _ -> Alcotest.fail "hcat padding for large, smol should pad smol"

let shift_stitch_down () =
        let displaced_down = Compose_stitch.shift_stitches_down ~amount:1 (List.hd Patterns.smol.layers) in
        Alcotest.(check int) "only one stitch after shifting down" (CoordinateSet.cardinal displaced_down.stitches) 1;
        (* TODO: this doesn't test backstitch *)
        let displaced_pattern = {layers = [displaced_down]; substrate = Patterns.big.substrate; backstitch_layers = []} in
        has_stitch ~name:"displaced single smol stitch" displaced_pattern (1, 2);
        n_stitches 1 displaced_pattern

let shift_stitch_right () =
        let displaced_right = Compose_stitch.shift_stitches_right ~amount:1 (List.hd Patterns.smol.layers) in
        Alcotest.(check int) "only one stitch after shifting right" (CoordinateSet.cardinal displaced_right.stitches) 1;
        let displaced_pattern = {layers = [displaced_right]; substrate = Patterns.big.substrate; backstitch_layers = [] } in
        has_stitch ~name:"displaced single smol stitch" displaced_pattern (2, 1);
        n_stitches 1 displaced_pattern

let merge_unmergeable_layers () =
        let nonblack_thread = { Patterns.blackstitch with thread = List.nth Stitchy.DMC.Thread.basic 2; } in
        let nonmergeable_with_smol_layer = {nonblack_thread with stitches = CoordinateSet.singleton (1, 1)} in
        Alcotest.(check int) "smol pattern only has one layer" 1 (List.length Patterns.smol.layers);
        let merged = Stitchy.Layers.merge_threads [nonmergeable_with_smol_layer] Patterns.smol.layers in
        Alcotest.(check int) "two layers after attempted unmergeable merge" 2 (List.length merged)

let merge_mergeable_layers () =
        let merged = Stitchy.Layers.merge_threads Patterns.smol.layers Patterns.big.layers in
        Alcotest.(check int) "smol and big pattern use the same thread and should be mergeable" 1 (List.length merged);
        Alcotest.(check int) "stitches in merged layer" 3 (CoordinateSet.cardinal (List.hd merged).stitches)

let check_vcat () =
  let vcat_smol = Compose_stitch.vcat Patterns.smol Patterns.smol in
  substrate_size (5,1) vcat_smol;
  has_stitch ~name:"left stitches present" vcat_smol (1, 1);
  has_stitch ~name:"right stitches present" vcat_smol (4, 1);
  n_stitches 2 vcat_smol

let check_multiple_vcat () =
  Format.printf "%a" Stitchy.Types.pp_pattern Patterns.smol;
  let vcat_twosmol = Compose_stitch.vcat Patterns.smol Patterns.smol in
  Format.printf "%a" Stitchy.Types.pp_pattern vcat_twosmol;
  let vcat_moar = Compose_stitch.vcat vcat_twosmol Patterns.smol in
  substrate_size (8, 1) vcat_moar;
  Format.printf "%a" Stitchy.Types.pp_pattern vcat_moar;
  has_stitch ~name:"far left stitches present" vcat_moar (1, 1);
  has_stitch ~name:"middle stitches present" vcat_moar (4, 1);
  has_stitch ~name:"far right stitches present" vcat_moar (7, 1);
  n_stitches 3 vcat_moar;
  stitches_all_in_substrate vcat_moar

let unmergeable_vcat () =
  let orange_thread = List.nth Stitchy.DMC.Thread.basic 2 in
  let orange_layer = { Patterns.blackstitch with thread = orange_thread } in
  let unmergeable_layer = {orange_layer with stitches = CoordinateSet.singleton (0, 0)} in
  let left_pattern = {layers = [unmergeable_layer]; substrate = Patterns.smol.substrate; backstitch_layers = []} in
  let vcatted = Compose_stitch.vcat left_pattern Patterns.smol in
  n_stitches 2 vcatted;
  substrate_size (5, 1) vcatted;
  (
  match Stitchy.Types.stitches_at vcatted (1, 1) with
  | [] -> ()
  | _ -> Alcotest.fail "stitches at (1, 1), which there shouldn't be"
  );
  (
  match Stitchy.Types.stitches_at vcatted (0, 0) with
  | [] -> Alcotest.fail "no stitch at (0, 0)"
  | _::_::_ -> Alcotest.fail "too many stitches at (0, 0)"
  | stitch::[] ->
                  let stitch_color = snd stitch in
                  Alcotest.(check @@ bool) "stitch at (0, 0) is orange" (Stitchy.DMC.Thread.equal stitch_color orange_thread) true);
  match Stitchy.Types.stitches_at vcatted (4, 1) with
  | [] -> Alcotest.fail "no stitch at (4, 1)"
  | _::_::_ -> Alcotest.fail "too many stitches at (4, 1)"
  | stitch::[] ->
                  let stitch_color = snd stitch in
                  Alcotest.(check @@ bool) "stitch at (4, 1) is black" (Stitchy.DMC.Thread.equal stitch_color (List.hd Stitchy.DMC.Thread.basic)) true


let check_hcat () =
  let hcat_smol = Compose_stitch.hcat Patterns.smol Patterns.smol in
  substrate_size (2, 3) hcat_smol;
  has_stitch ~name:"top stitches present" hcat_smol (1, 1);
  has_stitch ~name:"bottom stitches present" hcat_smol (1, 3);
  n_stitches 2 hcat_smol

let check_multiple_hcat () =
  let hcat_smol = Compose_stitch.hcat Patterns.smol Patterns.smol in
  Format.printf "%a" Stitchy.Types.pp_pattern hcat_smol;
  let hcat_moar = Compose_stitch.hcat hcat_smol Patterns.smol in
  Format.printf "%a" Stitchy.Types.pp_pattern hcat_moar;
  substrate_size (2, 5) hcat_moar;
  has_stitch ~name:"far down stitches present" hcat_moar (1, 5);
  has_stitch ~name:"far top stitches present" hcat_moar (1, 1);
  has_stitch ~name:"middle stitches present" hcat_moar (1, 3)

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
        ("auxiliary functions", [
                Alcotest.test_case "hpadding function" `Quick no_vpadding;
                Alcotest.test_case "vpadding function" `Quick no_hpadding;
                Alcotest.test_case "padding hcat smol big" `Quick first_needs_padding_hcat;
                Alcotest.test_case "padding hcat big smol" `Quick second_needs_padding_hcat;
                Alcotest.test_case "down-shift a pattern" `Quick shift_stitch_down;
                Alcotest.test_case "right-shift a pattern" `Quick shift_stitch_right;
                Alcotest.test_case "merge unmergeable layers" `Quick merge_unmergeable_layers;
                Alcotest.test_case "merge mergeable layers" `Quick merge_mergeable_layers;
        ]);
    ("no padding", [Alcotest.test_case "same dimensions vcat" `Quick check_vcat;
                    Alcotest.test_case "multiple vcats" `Quick check_multiple_vcat;
                    Alcotest.test_case "unmergeable vcats" `Quick unmergeable_vcat;
                    Alcotest.test_case "same dimensions hcat" `Quick check_hcat;
                    Alcotest.test_case "multiple hcats" `Quick check_multiple_hcat;
                   ]);
    ("padding and centering needed", [
        Alcotest.test_case "big/smol vcat" `Quick bs_vcat;
        Alcotest.test_case "big/smol hcat" `Quick bs_hcat;

      ]);
  ]
