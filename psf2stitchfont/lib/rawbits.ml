let to_bit_list byte =
  List.init 8 (fun index ->
      let mask = 1 lsl index in
      (Char.code byte) land mask != 0
    ) |> List.rev

let to_bit_stream bytes =
  let rec aux (l : bool list list) (bytes : Cstruct.t) =
    if Cstruct.len bytes <= 0 then (List.rev l) |> List.flatten
    else aux ((to_bit_list (Cstruct.get_char bytes 0))::l) (Cstruct.shift bytes 1)
  in
  aux [] bytes

let rec first l acc = function
  | n when n <= 0 || List.length l = 0 -> (List.rev acc), l
  | n -> first (List.tl l) ((List.hd l)::acc) (n-1)

let get_chars ~width ~height all_bits =
  let rec one_char ~y (pattern : (int * int) list) (next_bits : bool list) =
    if y >= height then (pattern, next_bits) else begin
      let row, rest = first next_bits [] width in
      let indices = List.fold_left (fun (acc, n) bit -> if bit then (n::acc, n+1) else (acc, n+1)) ([], 0) row |> fst |> List.rev in
      let coordinates = List.map (fun x -> (x, y)) indices in
      one_char ~y:(y+1) (pattern @ coordinates) rest
    end
  in
  let rec all_chars glyphs bits =
    if (List.length bits < width * height) then List.rev glyphs
    else begin
      let new_glyph, next_bits = one_char ~y:0 [] bits in
      all_chars (new_glyph::glyphs) next_bits
    end
  in
  all_chars [] all_bits

let glyphs_of_bytes ~width ~height bytes =
  get_chars ~width ~height (to_bit_stream bytes)
