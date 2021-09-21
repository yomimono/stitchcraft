let max3 a b c : int = max (max a b) (max b c)

let border_repetitions ~fencepost ~center ~side = match center mod (side + fencepost) with
  (* since we need to insert another fencepost anyway, leftovers <= the size of the last fencepost are nothing to worry about *)
  | w when w <= fencepost -> center / (side + fencepost)
  | _ -> (* too much space left over; add another repetition *)
    center / (side + fencepost) + 1
