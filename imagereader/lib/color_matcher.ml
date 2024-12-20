(* so over here, we get an `RGBMap.t` and we expect to map those to a `Stitchy.Types.layer list`.
 *
 * layers need a thread, a stitch, and a list of coordinates.
 * we can ask for a stitch to assume (it's going to be Full Cross basically all the time),
 * and the list of coordinates is just the value for each key in the RGBMap.
 * the tricky part is mapping the RGB to a thread,
 * something I have done my damnest to avoid for seven years,
 * but which seems to have finally caught up with me. *)

(* for now, we support only one palette - that which is carried around in code in
 * the Stitchy DMC module. *)

module RGBMap = Map.Make(Stitchy.RGB)

let closest rgb =
  let metric t = Stitchy.RGB.redmean rgb (Stitchy.DMC.Thread.to_rgb t) in
  let probably_bad_match = List.hd Stitchy.DMC.Thread.basic in
  let best, _ =
    List.fold_left (fun (best_thread, best_metric) candidate ->
      let m = metric candidate in
      if Float.compare m best_metric < 0 then (candidate, m)
      else (best_thread, best_metric)
    ) (probably_bad_match, metric probably_bad_match) Stitchy.DMC.Thread.all
  in
  best

let translate ~debug ~stitch (colors: (int * int) list RGBMap.t) : Stitchy.Types.layers =
  let l = RGBMap.to_list colors in
  List.fold_left (fun layers (rgb, coordinates) ->
      let open Stitchy.Types in
      let thread = closest rgb in
      if debug then Format.printf "we think %a is closest to %a\n%!" Stitchy.Types.pp_thread thread Stitchy.RGB.pp rgb;
      { thread; stitch; stitches = CoordinateSet.of_list coordinates;}::layers
    ) [] l
