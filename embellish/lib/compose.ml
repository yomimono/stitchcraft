let max3 a b c : int = max (max a b) (max b c)

let border_repetitions ~center ~side = match center mod side with
  | 0 -> center / side
  | _ -> (center / side) + 1
