type t = (int * int * int)
[@@deriving yojson, eq]

let to_crowbar = Crowbar.(map [int; int; int] (fun a b c -> a, b, c))

let compare (a, b, c) (x, y, z) =
 match compare a x with
 | 0 -> begin match compare b y with
     | 0 -> compare c z
     | n -> n
   end
 | n -> n
