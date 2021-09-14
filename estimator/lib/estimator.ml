open Stitchy.Types

module ThreadMap = Map.Make(Stitchy.DMC.Thread)

(* TODO metric!!! *)

(* TODO this is for white fourteen-count; it might be different for different sizes and colors *)
let aida_price_per_square_inch =
  let price_per_yard = 20.
  and square_inches = 36. *. 60.
  in
  price_per_yard /. square_inches

(* There is a thread length below which it's not possible to make meaningful stitches.
* TODO this would properly take the number of contiguous regions into account
 * (confetti makes this problem worse) *)
let minimum_thread_length = 6.

(* TODO need some estimates on whether thread is divided into 3s or 2s; for now just assume we use 3 strands everywhere *)
let strand_division_factor = 2. (* for each inch of six-strand embroidery floss we get two inches of three-strand embroidery floss *)
let price_per_skein = 1.00 (* they're .77 on DMC's website and often around this in stores but scarcity is lurking *)
let skein_length_inches = 8.7 *. 36. *. strand_division_factor (* 8m (8.7 yd) according to the label *)

let thread_fudge_factor = 1.2 (* extra thread for estimates, for anchoring etc *)

let seconds_per_stitch = 10 (* I have no idea what the right value is here *)

let count grid = match grid with
  | Fourteen -> 14
  | Sixteen -> 16
  | Eighteen -> 18

let substrate_size_in_inches ~margin_inches substrate =
  let count = count substrate.grid in
  let double_margin = 2. *. margin_inches in
  let inches dim =
    let inches_for_center dim =
      (dim / count) + (if 0 = dim mod count then 0 else 1)
    in
    double_margin +. (float_of_int @@ inches_for_center dim)
  in
  let width, height = substrate.max_x + 1, substrate.max_y + 1 in
  (inches width, inches height)

let hoop_size substrate =
  let width_in, height_in = substrate_size_in_inches ~margin_inches:0. substrate in
  let raw_size = int_of_float @@ (max width_in height_in) +. 1. in
  if raw_size > 12 then
    `Scroll_frame (int_of_float @@ (min width_in height_in) +. 1.)
  else
    `Embroidery_hoop (if raw_size > 4 then raw_size else 4)

let pp_hoop_size fmt = function
  | `Scroll_frame size -> Format.fprintf fmt "a scroll frame with small dimension of %d inches" size
  | `Embroidery_hoop size -> Format.fprintf fmt "an embroidery hoop with diameter %d inches" size

let smallest_frame ~margin_inches substrate =
  let width_in, height_in = substrate_size_in_inches ~margin_inches substrate in
  let biggest_dimension = max width_in height_in in
  let smallest_dimension = min width_in height_in in
  let fits (s, b) frame_small frame_large =
    let proportions ratio_a ratio_b =
      0 = compare (int_of_float ratio_a) (int_of_float ratio_b)
    in
    let small_prop = frame_small /. s
    and large_prop = frame_large /. b
    in
    s < frame_small && b < frame_large && proportions small_prop large_prop
  in
  let frame s b = `Frame (s, b) in
  (* custom frames are available in arbitrary sizes,
   * but it can be convenient to know the smallest
   * standard frame size a completed work
   * will fit in. *)
  match smallest_dimension, biggest_dimension with
  | size when fits size 5. 7. -> frame 5. 7.
  | size when fits size 8. 10. -> frame 8. 10.
  | size when fits size 8.5 11. -> frame 8.5 11.
  | size when fits size 11. 14. -> frame 11. 14.
  | size when fits size 11. 17. -> frame 11. 17.
  | size when fits size 16. 20. -> frame 16. 20.
  | size when fits size 18. 24. -> frame 18. 24.
  | size when fits size 20. 24. -> frame 20. 24.
  | size when fits size 24. 36. -> frame 24. 36.
  | size -> `Custom_frame size

let pp_frame fmt = function
  | `Frame (s, b) -> Format.fprintf fmt "standard %.1f x %.1f frame" s b
  | `Custom_frame (s, b) -> Format.fprintf fmt "custom frame, of minimum size %.1f x %.1f" s b
 
(* 2 * sqrt(2) + 1 is 3.8 which is close enough to 4 *)
(* logic: if the X crosses a unit square, the length from corner to corner is sqrt(1^2 + 1^2) = sqrt(2), and we do it twice, plus one stitch on a unit side to get us from one leg to the other. *)
let stitch_length_units = 4

(* TODO a better estimate would make some reference to region continuity --
   we need more thread for a bunch of unconnected stitches. for now, just
   multiply by a constant factor *)
(* also TODO: this assumes every stitch is a full cross stitch *)
let full_cross_length stitch_count grid_size =
  let grid_size = count grid_size in
  (* this is in [grid_size]ths of an inch, so divide it to get the size in inches *)
  let l = stitch_length_units * stitch_count in
  let estimated = (float_of_int l) /. (float_of_int grid_size) *. thread_fudge_factor in
  max estimated minimum_thread_length

(* give a maximum estimation of the amount of thread a person might reasonably use to make backstitches
 * traversing the distance from src to dst.
 * Notably this *isn't* a measure of distance from src to dst -
 * it's the city-block distance (i.e., no diagonals), doubled to
 * account for needing to do the "back" part of "backstitch". *)
let backstitch_length (src_x, src_y) (dst_x, dst_y) =
  let x_distance = abs (dst_x - src_x)
  and y_distance = abs (dst_y - src_y)
  in
  2 * (x_distance + y_distance)

let backstitch_layer_length (stitches : Stitchy.Types.SegmentSet.t) =
  Stitchy.Types.SegmentSet.fold (fun (src, dst) acc  ->
      acc + (backstitch_length src dst)
    ) stitches 0

type thread_info = {
  thread : Stitchy.DMC.Thread.t;
  amount : int;
  length : float;
  skeins : float;
  cost : float;
  seconds : int;
}

type materials = {
  threads : thread_info list;
  fabric : float * float;
}

let totals threads =
  List.fold_left (fun (total_cost, total_seconds) {cost; seconds; _} ->
     (total_cost +. cost, total_seconds + seconds)) (0., 0) threads

let print_thread_info {thread; amount; length; skeins; cost; seconds } =
  Printf.printf "%s: %d stitches (%.02f linear inches, %.02f standard skeins, USD %.02f, ~%d seconds)\n%!"
    (Stitchy.DMC.Thread.to_string thread) amount length skeins cost seconds

let backstitch_thread_info grid (layer : backstitch_layer) =
  let thread = layer.thread in
  let amount = backstitch_layer_length layer.stitches in
  (* each unit accounted for in `amount` is (1/grid_size) inches, so divide by the grid size to get the
   * length in inches *)
  let length = (float_of_int amount) /. (float_of_int @@ count grid) in
  let skeins = length /. skein_length_inches in
  let cost = skeins *. price_per_skein in
  let seconds = seconds_per_stitch * amount in
  {thread; amount; length; skeins; cost; seconds;}

let cross_thread_info grid (layer : layer) =
  let thread = layer.thread in
  let amount = CoordinateSet.cardinal layer.stitches in
  let length = full_cross_length amount grid in (* TODO: nope. there are lots of stitch types *)
  let skeins = length /. skein_length_inches in
  let cost = skeins *. price_per_skein in
  let seconds = seconds_per_stitch * amount in
  {thread; amount; length; skeins; cost; seconds;}

let materials ~margin_inches pattern =
  let cross_threads = List.map (cross_thread_info pattern.substrate.grid) pattern.layers in
  let backstitch_threads = List.map (backstitch_thread_info pattern.substrate.grid) pattern.backstitch_layers in
  { threads = cross_threads @ backstitch_threads;
    fabric = substrate_size_in_inches ~margin_inches pattern.substrate }
