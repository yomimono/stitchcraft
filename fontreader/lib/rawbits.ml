let to_bit_list byte =
  List.init 8 (fun index ->
      let mask = 1 lsl index in
      (Char.code byte) land mask != 0
    ) |> List.rev

let to_bit_stream bytes =
  let rec aux (l : bool list list) (bytes : Cstruct.t) =
    if Cstruct.length bytes <= 0 then (List.rev l) |> List.flatten
    else aux ((to_bit_list (Cstruct.get_char bytes 0))::l) (Cstruct.shift bytes 1)
  in
  aux [] bytes

let split_list l n =
  let rec aux l acc = function
  | n when n <= 0 || List.length l = 0 -> (List.rev acc), l
  | n -> aux (List.tl l) ((List.hd l)::acc) (n-1)
  in
  aux l [] n

let pp_bit fmt = function
  | true -> Format.fprintf fmt "X"
  | false -> Format.fprintf fmt " "

let rows_bitwise y (acc, n) bit =
  if bit then ((n, y)::acc, n+1)
  else (acc, n+1)

let get_nth_char_bits ~width ~height buf n =
  let char_size_bytes = (width * height / 8) + if width * height mod 8 > 0 then 1 else 0 in
  let offset_byte_index = char_size_bytes * n in
  (* after quite a bit of head-scratching, it looks like non-byte-aligned characters still get padded
     so they begin on a byte boundary. *)
  let relevant_bytes = Cstruct.sub buf offset_byte_index char_size_bytes in
  let bits = to_bit_stream relevant_bytes in
  (* this is kind of wild, but it seems like we get some unneeded padding bits in 9x14 characters finishing out the 9th line (y=8).
     They appear to be at the 6th bit (x=5).  (9*8 = 72, plus 5 is 77, which doesn't seem special to me, but what do I know.) *)
  let uneven_adjusted =
    if width = 9 && height = 14 then begin
    let pre_pad, post_pad = split_list bits (9*8 + 5) in
    let post_pad_less_4 = snd @@ split_list post_pad 0 in
    pre_pad @ post_pad_less_4
  end else fst @@ split_list bits (width * height) in
  uneven_adjusted

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

let get_chars _debug ~width ~height (all_bytes : Cstruct.t) =
  let rec aux (n, acc) =
    (* if we're out of buffer, stop *)
    let size_in_bytes = (width * height / 8) + if width * height mod 8 > 0 then 1 else 0 in
    if (n + 1) * size_in_bytes > (Cstruct.length all_bytes) then (n, List.rev acc)
    else begin
      let bits = get_nth_char_bits ~width ~height all_bytes n in
      let stitches = stitches_of_bits ~width ~height bits in
      aux (n+1, stitches :: acc)
    end
  in
  aux (0, [])

(* width and height are in bits *)
let glyphs_of_bytes debug ~width ~height bytes =
  snd @@ get_chars debug ~width ~height bytes
