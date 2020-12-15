open Stitchy.Types

module ThreadMap = Map.Make(Stitchy.DMC.Thread)

(* TODO metric!!! *)

(* TODO this is for white fourteen-count; it might be different for different sizes and colors *)
let aida_price_per_square_inch =
  let price_per_yard = 20.
  and square_inches = 36. *. 60.
  in
  price_per_yard /. square_inches

(* TODO need some estimates on whether thread is divided into 3s or 2s; for now just assume we use 3 strands everywhere *)
let strand_division_factor = 2. (* for each inch of six-strand embroidery floss we get two inches of three-strand embroidery floss *)
let price_per_skein = 0.80 (* they're .77 on DMC's website and often around this in stores *)
let skein_length_inches = 8.7 *. 36. *. strand_division_factor (* 8m (8.7 yd) according to the label *)

let thread_fudge_factor = 1.2 (* extra thread for estimates, for anchoring etc *)

let seconds_per_stitch = 10 (* I have no idea what the right value is here *)

let count grid = match grid with
  | Fourteen -> 14
  | Sixteen -> 16
  | Eighteen -> 18

let substrate_size_in_inches ?(margin_inches=1.) substrate =
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

let stitches_per_color stitches =
  BlockMap.fold (fun _ block acc ->
      match ThreadMap.find_opt block.thread acc with
      | None -> ThreadMap.add block.thread 1 acc
      | Some n -> ThreadMap.add block.thread (n+1) acc
    ) stitches ThreadMap.empty

(* 2 * sqrt(2) + 2 is 4.8 which is close enough to 5 *)
let stitch_length_units = 5

(* TODO a better estimate would make some reference to region continuity --
   we need more thread for a bunch of unconnected stitches. for now, just
   multiply by a constant factor *)
let length_of_thread stitch_count grid_size =
  let grid_size = count grid_size in
  (* this is in [grid_size]ths of an inch, so divide it to get the size in inches *)
  let l = stitch_length_units * stitch_count in
  (float_of_int l) /. (float_of_int grid_size) *. thread_fudge_factor

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
  Printf.printf "%s: %d stitches (%.02G linear inches, %.02G standard skeins, USD %G, ~%d seconds)\n%!"
    (Stitchy.DMC.Thread.to_string thread) amount length skeins cost seconds

let thread_info grid threads =
  ThreadMap.fold (fun thread amount l ->
      let length = length_of_thread amount grid in
      let skeins = length /. skein_length_inches in
      let cost = skeins *. price_per_skein in
      let seconds = seconds_per_stitch * amount in
      {thread; amount; length; skeins; cost; seconds;} :: l) threads []

let materials state =
  { threads = thread_info state.substrate.grid (stitches_per_color state.stitches);
    fabric = substrate_size_in_inches state.substrate }
