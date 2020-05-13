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
