type t = (int * int * int)
[@@deriving yojson, eq]

let pp fmt (r, g, b) = Format.fprintf fmt "%d, %d, %d" r g b

let compare (a, b, c) (x, y, z) =
 match compare a x with
 | 0 -> begin match compare b y with
     | 0 -> compare c z
     | n -> n
   end
 | n -> n

(* 'redmean', from https://en.wikipedia.org/wiki/Color_difference *)
let redmean (a, b, c) (r1, r2, r3) =
  let diff_and_conv l r = Int.to_float (l - r) in
  let bar_r = (Int.to_float (a + r1)) /. 2. in
  let delta_r = diff_and_conv a r1
  and delta_g = diff_and_conv b r2
  and delta_b = diff_and_conv c r3
  in
  Float.sqrt (
    ((2. +. (bar_r /. 256.)) *. (Float.pow delta_r 2.)) +.
    (4. *. (Float.pow delta_g 2.)) +.
    ((2. +. ((255. -. bar_r) /. 256.)) *. (Float.pow delta_b 2.))
  )
