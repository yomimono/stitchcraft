(* fonts, as we will consider them, inhabit boxes of size x * y, are not
   anti-aliased, and can have variable intercharacter and interline spacing. *)

module CharMap = Map.Make(Char)

(* some useful dimensions to have access to from a phrase *)
type dimensions = {
  lines : string list;
  height : int;
  width : int;
}

let h, w = 8, 8 (* c64 chars are 8x8 *)

let get_dimensions phrase interline =
  (* make the user figure out the line wrapping for us, for the moment. *)
  let lines = Astring.String.cuts ~sep:"\n" phrase in
  let height = (List.length lines) * h in
  let interline = (max 0 (List.length lines - 1)) * interline in
  let longest_line =
    List.fold_left (fun longest s ->
        max (Astring.String.length s) longest) 0 lines
  in
  let width = longest_line * w in
  { lines;
    height = height + interline;
    width }

let left_bar = [
  (1, 1); (2, 1);
  (1, 2); (2, 2);
  (1, 3); (2, 3);
  (1, 4); (2, 4);
  (1, 5); (2, 5);
  (1, 6); (2, 6);
]

let right_bar = List.map (fun (x, y) -> (x + 4, y)) left_bar

(*
+--------+
| xx     |
| xx     |
| xx     |
| xx     |
| xx     |
| xx     |
| xx     |
|        |
+--------+
 *)
let upper_left_bar = left_bar @ [
    (1, 0); (2, 0);
  ]

let upper_right_bar = right_bar @ [
    (5, 0); (6, 0);
  ]

(*
+--------+
|   xx   |
|  xxxx  |
| xx  xx |
| xxxxxx |
| xx  xx |
| xx  xx |
| xx  xx |
|        |
+--------+
 *)
let upper_a =
  [(3, 0); (4, 0);
   (2, 1); (3, 1); (4, 1); (5, 1);
   (1, 2); (2, 2); (5, 2); (6, 2);
   (1, 3); (2, 3); (3, 3); (4, 3); (5, 3); (6, 3);
   (1, 4); (2, 4); (5, 4); (6, 4);
   (1, 5); (2, 5); (5, 5); (6, 5);
   (1, 6); (2, 6); (5, 6); (6, 6);
  ]

(*
+--------+
| xxxxx  |
| xx  xx |
| xx  xx |
| xxxxx  |
| xx  xx |
| xx  xx |
| xxxxx  |
|        |
+--------+
 *)
let upper_b =
  upper_left_bar @
  [ (3, 0); (4, 0); (5, 0);
    (5, 1); (6, 1);
    (5, 2); (6, 2);
    (3, 3); (4, 3); (5, 3);
    (5, 4); (6, 4);
    (5, 5); (6, 5);
    (3, 6); (4, 6); (5, 6);
  ]

let upper_c =
  [
    (2, 0); (3, 0); (4, 0); (5, 0);
    (1, 1); (2, 1); (5, 1); (6, 1);
    (1, 2); (2, 2);
    (1, 3); (2, 3);
    (1, 4); (2, 4);
    (1, 5); (2, 5); (5, 5); (6, 5);
    (2, 6); (3, 6); (4, 6); (5, 6);
  ]

let upper_d =
  upper_left_bar @ [
    (3, 0); (4, 0);
    (4, 1); (5, 1);
    (5, 2); (6, 2);
    (5, 3); (6, 3);
    (5, 4); (6, 4);
    (4, 5); (5, 5);
    (3, 6); (4, 6);
  ]

let upper_f =
  upper_left_bar @
  [ (3, 0); (4, 0); (5, 0);
    (3, 3); (4, 3);
  ]

let upper_e = upper_f @ [
    (3, 6); (4, 6); (5, 6);
  ]

let upper_g = upper_c @ [
    (4, 3); (5, 3); (6, 3);
    (5, 4); (6, 4);
  ]

let upper_h =
  upper_left_bar @ upper_right_bar @ [
  (3, 3); (4, 3);
]

let upper_i = List.map (fun (x, y) -> (x + 2, y)) upper_left_bar @ [
  (2, 0); (5, 0);
  (2, 6); (5, 6);
]

let upper_j = [
  (3, 0); (4, 0); (5, 0); (6, 0);
  (4, 1); (5, 1);
  (4, 2); (5, 2);
  (4, 3); (5, 3);
  (4, 4); (5, 4);
  (1, 5); (2, 5); (4, 5); (5, 5);
  (2, 6); (3, 6); (4, 6);
]

let upper_k =
  upper_left_bar @ [
    (5, 0); (6, 0);
    (4, 1); (5, 1);
    (3, 2); (4, 2);
    (3, 3);
    (3, 4); (4, 4);
    (4, 5); (5, 5);
    (5, 6); (6, 6);
  ]

let upper_l =
  upper_left_bar @ [
    (3, 6); (4, 6); (5, 6); (6, 6);
  ]

let upper_m =
  upper_left_bar @ List.map (fun (x, y) -> (x + 5, y)) upper_left_bar @ [
    (3, 1); (5, 1);
    (3, 2); (4, 2); (5, 2);
    (4, 3);
  ]

let upper_n =
  upper_left_bar @ upper_right_bar @ [
    (3, 1);
    (3, 2); (4, 2);
    (3, 3); (4, 3);
    (4, 4);
  ]

let upper_o = [
  (2, 0); (3, 0); (4, 0); (5, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (2, 6); (3, 6); (4, 6); (5, 6);
]

let upper_p = upper_left_bar @ [
    (3, 0); (4, 0); (5, 0);
    (5, 1); (6, 1);
    (5, 2); (6, 2);
    (3, 3); (4, 3); (5, 3);
  ]

let upper_q = [
  (2, 0); (3, 0); (4, 0); (5, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (2, 5); (3, 5); (4, 5); (5, 5);
  (4, 6); (5, 6); (6, 6);
]

let upper_r = upper_p @ [
    (3, 4); (4, 4);
    (4, 5); (5, 5);
    (5, 6); (6, 6);
  ]

let upper_s = [
  (2, 0); (3, 0); (4, 0); (5, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (1, 2); (2, 2);
  (2, 3); (3, 3); (4, 3); (5, 3);
  (5, 4); (5, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (2, 6); (3, 6); (4, 6); (5, 6);
]

let upper_t = List.map (fun (x, y) -> (x + 2, y)) upper_left_bar @ [
    (1, 0); (2, 0); (5, 0); (6, 0);
  ]

let upper_u = [
  (1, 0); (2, 0); (5, 0); (6, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (2, 6); (3, 6); (4, 6); (5, 6);
]

let upper_v = [
  (1, 0); (2, 0); (5, 0); (6, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (2, 5); (3, 5); (4, 5); (5, 5);
  (3, 6); (4, 6);
]

let upper_w = List.map (fun (x, y) -> (x, 6 - y)) upper_m

let upper_x = [
  (1, 0); (2, 0); (5, 0); (6, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (2, 2); (3, 2); (4, 2); (5, 2);
  (3, 3); (4, 3);
  (2, 4); (3, 4); (4, 4); (5, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (1, 6); (2, 6); (5, 6); (6, 6);
]

let upper_y = [
  (1, 0); (2, 0); (5, 0); (6, 0);
  (1, 1); (2, 1); (5, 1); (6, 1);
  (2, 2); (3, 2); (4, 2); (5, 2);
  (3, 3); (4, 3);
  (3, 4); (4, 4);
  (3, 5); (4, 5);
  (3, 6); (4, 6);
]

let upper_z = [
  (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0);
  (5, 1); (6, 1);
  (4, 2); (5, 2);
  (3, 3); (4, 3);
  (2, 4); (3, 4);
  (1, 5); (2, 5);
  (1, 6); (2, 6); (3, 6); (4, 6); (5, 6); (6, 6);
]

let lower_a = [
  (2, 2); (3, 2); (4, 2); (5, 2);
  (5, 3); (6, 3);
  (2, 4); (3, 4); (4, 4); (5, 4); (6, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (2, 6); (3, 6); (4, 6); (5, 6); (6, 6);
]

let lower_b = left_bar @ [
    (3, 3); (4, 3); (5, 3);
    (5, 4); (6, 4);
    (5, 5); (6, 5);
    (3, 6); (4, 6); (5, 6);
  ]

let lower_c = [
  (2, 2); (3, 2); (4, 2); (5, 2);
  (1, 3); (2, 3);
  (1, 4); (2, 4);
  (1, 5); (2, 5);
  (2, 6); (3, 6); (4, 6); (5, 6);
]

let lower_d = List.map (fun (x, y) ->
    (7 - x, y)) lower_b

let lower_e = lower_c @ [
    (5, 2);
    (5, 3); (6, 3);
    (3, 4); (4, 4); (5, 4); (6, 4);
  ]

let lower_f = [
  (3, 1); (4, 1); (5, 1);
  (2, 2); (3, 2);
  (1, 3); (2, 3); (3, 3); (4, 3); (5, 3);
  (2, 4); (3, 4);
  (2, 5); (3, 5);
  (2, 6); (3, 6);
]

let lower_g = [
  (2, 2); (3, 2); (4, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (2, 5); (3, 5); (4, 5); (5, 5); (6, 5);
  (5, 6); (6, 6);
  (1, 7); (2, 7); (3, 7); (4, 7); (5, 7);
]

let lower_h = left_bar @ [
    (3, 3); (4, 3); (5, 3);
    (5, 4); (6, 4);
    (5, 5); (6, 5);
    (5, 6); (6, 6);
  ]

let lower_i = [
  (3, 1); (4, 1);
  (2, 3); (3, 3); (4, 3);
  (3, 4); (4, 4);
  (3, 5); (4, 5);
  (2, 6); (3, 6); (4, 6); (5, 6);
]

let lower_j = [
  (5, 1); (6, 1);
  (5, 3); (6, 3);
  (5, 4); (6, 4);
  (5, 5); (6, 5);
  (5, 6); (6, 6);
  (2, 7); (3, 7); (4, 7); (5, 7);
]

let lower_k = left_bar @ [
    (4, 3); (5, 3);
    (3, 4); (4, 4);
    (4, 5); (5, 5);
    (5, 6); (6, 6);
  ]

let lower_l = List.map (fun (x, y) -> (x + 2, y)) left_bar @ [
    (2, 1);
    (2, 6); (5, 6);
  ]

let lower_m = [
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (3, 3); (4, 3); (5, 3); (6, 3); (7, 3);
  (1, 4); (2, 4); (3, 4); (4, 4); (5, 4); (6, 4); (7, 4);
  (1, 5); (2, 5); (4, 5); (6, 5); (7, 5);
  (1, 6); (2, 6); (6, 6); (7, 6);
]

let lower_n = [
  (1, 2); (2, 2); (3, 2); (4, 2); (5, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (1, 5); (2, 5); (5, 5); (6, 5);
  (1, 6); (2, 6); (5, 6); (6, 6);
]

let lower_o = lower_c @ [
  (5, 3); (6, 3);
  (5, 4); (6, 4);
  (5, 5); (6, 5);
]

(* p is inverted b moved down one pixel *)
let lower_p = List.map (fun (x, y) -> (x, (8 - y))) lower_b

let lower_q = List.map (fun (x, y) -> (7 - x, y)) lower_p

let lower_r = [
  (1, 2); (2, 2); (3, 2); (4, 2); (5, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4);
  (1, 5); (2, 5);
  (1, 6); (2, 6);
]

let lower_s = [
  (2, 2); (3, 2); (4, 2); (5, 2); (6, 2);
  (1, 3); (2, 3);
  (2, 4); (3, 4); (4, 4); (5, 4);
  (5, 5); (6, 5);
  (1, 6); (2, 6); (3, 6); (4, 6); (5, 6);
]

let lower_t = [
  (3, 1); (4, 1);
  (1, 2); (2, 2); (3, 2); (4, 2); (5, 2); (6, 2);
  (3, 3); (4, 3);
  (3, 4); (4, 4);
  (3, 5); (4, 5);
  (4, 6); (5, 6);
]

let lower_u = List.map (fun (x, y) -> (7 - x, 8 - y)) lower_n

let lower_v = [
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (2, 5); (3, 5); (4, 5); (5, 5);
  (3, 6); (4, 6);
]

let lower_w = [
  (1, 2); (2, 2); (6, 2); (7, 2);
  (1, 3); (2, 3); (4, 3); (6, 3); (7, 3);
  (2, 4); (3, 4); (4, 4); (5, 4); (6, 4); (7, 4);
  (2, 5); (3, 5); (4, 5); (5, 5); (6, 5);
  (2, 6); (3, 6); (5, 6); (6, 6);
]

let lower_x = [
  (1, 2); (2, 2); (5, 2); (6, 2);
  (2, 3); (3, 3); (4, 3); (5, 3);
  (3, 4); (4, 4);
  (2, 5); (3, 5); (4, 5); (5, 5);
  (1, 6); (2, 6); (5, 6); (6, 6);
]

let lower_y = [
  (1, 2); (2, 2); (5, 2); (6, 2);
  (1, 3); (2, 3); (5, 3); (6, 3);
  (1, 4); (2, 4); (5, 4); (6, 4);
  (2, 5); (3, 5); (4, 5); (5, 5); (6, 5);
  (4, 6); (5, 6);
  (1, 7); (2, 7); (3, 7); (4, 7);
]

let lower_z = [
  (1, 2); (2, 2); (3, 2); (4, 2); (5, 2); (6, 2);
  (4, 3); (5, 3);
  (3, 4); (4, 4);
  (2, 5); (3, 5);
  (1, 6); (2, 6); (3, 6); (4, 6); (5, 6); (6, 6);
]

let backslash =
  [
    (0, 0); (0, 1);
    (1, 0); (1, 1); (1, 2);
    (2, 1); (2, 2); (2, 3);
    (3, 2); (3, 3); (3, 4);
    (4, 3); (4, 4); (4, 5);
    (5, 4); (5, 5); (5, 6);
    (6, 5); (6, 6); (6, 7);
    (7, 6); (7, 7);
  ]

let forward_slash = List.map (fun (x, y) -> (7 - x), y) backslash

let map =
  let m = CharMap.empty in
  let m = CharMap.add 'A' upper_a m in
  let m = CharMap.add 'B' upper_b m in
  let m = CharMap.add 'C' upper_c m in
  let m = CharMap.add 'D' upper_d m in
  let m = CharMap.add 'E' upper_e m in
  let m = CharMap.add 'F' upper_f m in
  let m = CharMap.add 'G' upper_g m in
  let m = CharMap.add 'H' upper_h m in
  let m = CharMap.add 'I' upper_i m in
  let m = CharMap.add 'J' upper_j m in
  let m = CharMap.add 'K' upper_k m in
  let m = CharMap.add 'L' upper_l m in
  let m = CharMap.add 'M' upper_m m in
  let m = CharMap.add 'N' upper_n m in
  let m = CharMap.add 'O' upper_o m in
  let m = CharMap.add 'P' upper_p m in
  let m = CharMap.add 'Q' upper_q m in
  let m = CharMap.add 'R' upper_r m in
  let m = CharMap.add 'S' upper_s m in
  let m = CharMap.add 'T' upper_t m in
  let m = CharMap.add 'U' upper_u m in
  let m = CharMap.add 'V' upper_v m in
  let m = CharMap.add 'W' upper_w m in
  let m = CharMap.add 'X' upper_x m in
  let m = CharMap.add 'Y' upper_y m in
  let m = CharMap.add 'Z' upper_z m in
  let m = CharMap.add 'a' lower_a m in
  let m = CharMap.add 'b' lower_b m in
  let m = CharMap.add 'c' lower_c m in
  let m = CharMap.add 'd' lower_d m in
  let m = CharMap.add 'e' lower_e m in
  let m = CharMap.add 'f' lower_f m in
  let m = CharMap.add 'g' lower_g m in
  let m = CharMap.add 'h' lower_h m in
  let m = CharMap.add 'i' lower_i m in
  let m = CharMap.add 'j' lower_j m in
  let m = CharMap.add 'k' lower_k m in
  let m = CharMap.add 'l' lower_l m in
  let m = CharMap.add 'm' lower_m m in
  let m = CharMap.add 'n' lower_n m in
  let m = CharMap.add 'o' lower_o m in
  let m = CharMap.add 'p' lower_p m in
  let m = CharMap.add 'q' lower_q m in
  let m = CharMap.add 'r' lower_r m in
  let m = CharMap.add 's' lower_s m in
  let m = CharMap.add 't' lower_t m in
  let m = CharMap.add 'u' lower_u m in
  let m = CharMap.add 'v' lower_v m in
  let m = CharMap.add 'w' lower_w m in
  let m = CharMap.add 'x' lower_x m in
  let m = CharMap.add 'y' lower_y m in
  let m = CharMap.add 'z' lower_z m in
  let m = CharMap.add '\\' backslash m in
  let m = CharMap.add '/' forward_slash m in
  m
