(* if height and width aren't evenly divisible by 8,
   they'll be padded to fit (probably with 0s, since this is a bitmap font) *)
let pad_to_8 n =
  if n mod 8 = 0 then n
  else n + (8 - (n mod 8))

let high_bit_set byte = byte land 0x80 = 0x80

let to_bit_list byte =
  let rec aux l index byte = match (int_of_char byte) land 0xff with
    | 0 -> l
    | nonzero when high_bit_set nonzero ->
      let byte = (int_of_char byte) lsl 1 |> (land) 0xff |> char_of_int in
      aux (index :: l)  (index + 1) byte
    | _unset_bit ->
      let byte = (int_of_char byte) lsl 1 |> (land) 0xff |> char_of_int in
      aux l (index + 1) byte
  in
  aux [] 0 byte

let read_row ~y row ~width =
  let rec aux bits row index =
    if index >= width || Cstruct.len row = 0 then bits
    else begin
      let bit_list = to_bit_list (Cstruct.get_char row 0) in
      let new_bits = List.map (fun bit_number -> (bit_number + 8*index, y)) bit_list in
      aux (new_bits @ bits) (Cstruct.shift row 1) (index + 1)
    end
  in
  let stitches = aux [] row 0 in
  stitches

let rec read_rows buffer ~bytes_in_row ~height ~width rows index =
  if index >= height then rows
  else begin
    let row = read_row ~y:index (Cstruct.sub buffer 0 bytes_in_row) ~width in
    read_rows (Cstruct.shift buffer bytes_in_row) ~bytes_in_row ~height ~width (row @ rows) (index + 1)
  end

let bytes_in_row raw_width =
  if raw_width mod 8 <> 0 then raw_width / 8 + 1
  else raw_width / 8
