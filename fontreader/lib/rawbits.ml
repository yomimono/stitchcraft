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

let split_list l n =
  let rec aux l acc = function
  | n when n <= 0 || List.length l = 0 -> (List.rev acc), l
  | n -> aux (List.tl l) ((List.hd l)::acc) (n-1)
  in
  aux l [] n

let rows_bitwise y (acc, n) bit =
  if bit then ((n, y)::acc, n+1)
  else (acc, n+1)

let get_nth_char_bits ~width ~height buf n =
  let char_size_bits = width * height in
  let offset_bit_index = char_size_bits * n in
  let offset_byte_index = offset_bit_index / 8 in (* this is the offset # of the first byte we need *any* bits from *)
  let bit_offset_within_first_byte = (offset_bit_index mod 8) in
  let end_padding = if ((char_size_bits + bit_offset_within_first_byte) mod 8) > 0
    then 1 else 0 in
  let start_padding = if bit_offset_within_first_byte > 0 then 1 else 0 in
  let char_size_bytes = char_size_bits / 8 + start_padding + end_padding in
  let relevant_bytes = Cstruct.sub buf offset_byte_index char_size_bytes in
  let bits = to_bit_stream relevant_bytes in
  let _dropped_bits, offset_bits = split_list bits bit_offset_within_first_byte in
  let bits_we_need, _extra_bits = split_list offset_bits char_size_bits in
  bits_we_need

let stitches_of_bits ~width ~height bits =
  if (List.length bits < width * height) then []
  else begin
    List.fold_left (fun (stitches, n) bit ->
        let x = n mod width in
        let y = n / width in
        if bit then ((x, y)::stitches , n + 1)
        else (stitches, n + 1)
      ) ([], 0) bits |> fst
  end

let get_chars ~width ~height (all_bytes : Cstruct.t) =
  let rec aux (n, acc) =
    (* if we're out of buffer, stop *)
    if (((n + 1) * width * height) - 1) / 8 > (Cstruct.len all_bytes) then (n, List.rev acc)
    else begin
      let bits = get_nth_char_bits ~width ~height all_bytes n in
      let stitches = stitches_of_bits ~width ~height bits in
      aux (n+1, stitches :: acc)
    end
  in
  aux (0, [])

(* width and height are in bits *)
let glyphs_of_bytes ~width ~height bytes =
  snd @@ get_chars ~width ~height bytes
