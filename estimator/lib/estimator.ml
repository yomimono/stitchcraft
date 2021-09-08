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

(* 2 * sqrt(2) + 1 is 3.8 which is close enough to 4 *)
(* logic: if the X crosses a unit square, the length from corner to corner is sqrt(1^2 + 1^2) = sqrt(2), and we do it twice, plus one stitch on a unit side to get us from one leg to the other. *)
let stitch_length_units = 4

(* TODO a better estimate would make some reference to region continuity --
   we need more thread for a bunch of unconnected stitches. for now, just
   multiply by a constant factor *)
(* also TODO: this assumes every stitch is a full cross stitch *)
let length_of_thread stitch_count grid_size =
  let grid_size = count grid_size in
  (* this is in [grid_size]ths of an inch, so divide it to get the size in inches *)
  let l = stitch_length_units * stitch_count in
  let estimated = (float_of_int l) /. (float_of_int grid_size) *. thread_fudge_factor in
  max estimated minimum_thread_length

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

let thread_info grid (layer : layer) =
  let thread = layer.thread in
  let amount = CoordinateSet.cardinal layer.stitches in
  let length = length_of_thread amount grid in
  let skeins = length /. skein_length_inches in
  let cost = skeins *. price_per_skein in
  let seconds = seconds_per_stitch * amount in
  {thread; amount; length; skeins; cost; seconds;}

let materials pattern =
  { threads = List.map (thread_info pattern.substrate.grid) pattern.layers;
    fabric = substrate_size_in_inches ~margin_inches:1. pattern.substrate }
