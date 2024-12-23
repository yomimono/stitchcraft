(* so over here, we get an `RGBMap.t` and we expect to map those to a `Stitchy.Types.layer list`.
 *
 * layers need a thread, a stitch, and a list of coordinates.
 * we can ask for a stitch to assume (it's going to be Full Cross basically all the time),
 * and the set of stitches for each color is the value for each key in the RGBMap.
 * the tricky part is mapping the RGB to a thread,
 * something I have done my damnest to avoid for seven years,
 * but which seems to have finally caught up with me. *)

(* for now, we support only one palette - that which is carried around in code in
 * the Stitchy DMC module. *)

module RGBMap = Map.Make(Stitchy.RGB)

let oklab_dist a b =
  let labify (r, g, b) = Gg.Color.to_lab @@ Gg.Color.v_srgbi r g b in
  let lab_a = labify a
  and lab_b = labify b
  in
  let components t = Gg.V4.(x t, y t, z t) in
  let l1, a1, b1 = components lab_a
  and second_l, second_a, second_b = components lab_b
  in
  Float.sqrt @@
    Float.pow (l1 -. second_l) 2. +.
    Float.pow (a1 -. second_a) 2. +.
    Float.pow (b1 -. second_b) 2.

let closest ~algo rgb =
  (* let metric t = oklab_dist rgb (Stitchy.DMC.Thread.to_rgb t) in *)
  let metric t =
    match algo with
    | Stitchy.Types.Redmean -> Stitchy.RGB.redmean rgb (Stitchy.DMC.Thread.to_rgb t)
    | Oklab -> oklab_dist rgb (Stitchy.DMC.Thread.to_rgb t)
  in
  let probably_bad_match = List.hd Stitchy.DMC.Thread.basic in
  let best, _ =
    List.fold_left (fun (best_thread, best_metric) candidate ->
      let m = metric candidate in
      if Float.compare m best_metric < 0 then (candidate, m)
      else (best_thread, best_metric)
    ) (probably_bad_match, metric probably_bad_match) Stitchy.DMC.Thread.all
  in
  best

let translate ~debug ~stitch ~algo (colors: (int * int) list RGBMap.t) : Stitchy.Types.layers =
  let l = RGBMap.to_list colors in
  List.fold_left (fun layers (rgb, coordinates) ->
      let open Stitchy.Types in
      let thread = closest ~algo rgb in
      if debug then Format.printf "we think %a is closest to %a\n%!" Stitchy.Types.pp_thread thread Stitchy.RGB.pp rgb;
      { thread; stitch; stitches = CoordinateSet.of_list coordinates;}::layers
    ) [] l
