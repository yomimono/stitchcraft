module CharMap = Map.Make(Uchar)

(* some useful dimensions to have access to from a phrase *)
type dimensions = {
  lines : string list;
  height : int;
  width : int;
}

let h, w = 8, 8 (* c64 chars are 8x8 *)

let get_dimensions phrase interline =
  let decoder = Uutf.decoder s in
  let chars_until_newline d =
    let is_newline u = match Uchar.to_int u with
      | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 -> true
      | _ -> false
    in
    let rec advance d n =
      match Uutf.decode d with
      | `End -> (d, n)
      | `Uchar u when is_newline u ->
        (d, n)
      | `Uchar _ -> (d, advance d (n+1))
      | `Malformed _ | `Await -> (d, advance d n)
    in
    advance d 0
  in
  (* TODO: figure out how the lines are going.  We could get this from the decoder too,
     which keeps track of some internal state on which line and column of the text it's on. *)
  let lines d =
    let n_in_line = chars_until_newline d in
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

let filename c =
  (* TODO: big hack here, this needs to work differently *)
  let base = "c64chars" in
  (* some special logic for PETSCII *)
  match Uchar.is_char c with
  | true when Astring.Char.Ascii.is_lower @@ Uchar.to_char c ->
    Printf.sprintf "%s/%xl.png.layer0" base (int_of_char (Uchar.to_char c) - 0x20)
  | true when Astring.Char.Ascii.is_upper @@ Uchar.to_char c ->
    Printf.sprintf "%s/%xu.png.layer0" base (int_of_char (Uchar.to_char c))
  | true ->
    Printf.sprintf "%s/%x.png.layer0" base (int_of_char (Uchar.to_char c))
  | false ->
    let byte =
      (* mappings taken from https://style64.org/petscii *)
      match Uchar.to_int c with
      | 0x2500 | 0x2501 -> "40"
      | 0x2660 -> "41l"
      | 0x2502 | 0x007c | 0x2503 -> "42l"
      | 0x256e -> "49l"
      | 0x2570 -> "4al"
      | 0x256f -> "4bl"
      | 0x2572 -> "4dl"
      | 0x2571 -> "4el"
      | 0x2022 | 0x00b7 | 0x0025cf | 0x2219 -> "51l"
      | 0x2665 -> "53l"
      | 0x256d -> "55l"
      | 0x2573 -> "56l"
      | 0x25cb | 0x2218 | 0x25e6 | 0x25ef -> "57l"
      | 0x2663 -> "58l"
      | 0x2666 | 0x25c6 -> "59l"
      | 0x253c | 0x254b -> "5al"
      | 0x03c0 -> "5e"
      | 0x25e5 -> "5f"
      | 0x258c -> "61"
      | 0x2580 -> "62"
      | 0x2587 -> "63"
      | 0x005f | 0x2581 -> "64"
      | 0x258e -> "65"
      | 0x2595 -> "66"
      | 0x258a -> "67"
      | 0x251c | 0x2523 -> "6b"
      | 0x259b -> "6c"
      | 0x2514 | 0x2517 -> "6d"
      | 0x2510 | 0x2513 -> "6e"
      | 0x2582 -> "6f"
      | 0x250c | 0x250f -> "70"
      | 0x2534 | 0x253b -> "71"
      | 0x252c | 0x2533 -> "72"
      | 0x2524 | 0x252b -> "73"
      | 0x258b -> "76"
      | 0x2586 -> "77"
      | 0x2585 -> "78"
      | 0x2596 -> "7b"
      | 0x259d -> "7c"
      | 0x251b | 0x2518 -> "7d"
      | 0x2598 | 0x259f -> "7e"
      | 0x259e -> "7f"
      | _ -> ""
    in
    Printf.sprintf "%s/%s.png.layer0" base byte

let load_layer file =
  try Ok (Yojson.Safe.from_file file |> Stitchy.Types.layer_of_yojson)
  with s -> Error s

let maybe_add c m =
  match load_layer (filename c) with
  | Error _ | Ok (Error _) -> m
  | Ok (Ok layer) -> CharMap.add c layer m

let map =
  let m = ref CharMap.empty in
  for c = 20 to 255 do
    m := maybe_add (Uchar.of_int c) !m 
  done;
  let known_uchars = [
        0x2500 ; 0x2501
      ; 0x2660
      ; 0x2502 ; 0x007c ; 0x2503
      ; 0x256e
      ; 0x2570
      ; 0x256f
      ; 0x2572
      ; 0x2571
      ; 0x2022 ; 0x00b7 ; 0x0025cf ; 0x2219
      ; 0x2665
      ; 0x256d
      ; 0x2573
      ; 0x25cb ; 0x2218 ; 0x25e6 ; 0x25ef
      ; 0x2663
      ; 0x2666 ; 0x25c6
      ; 0x253c ; 0x254b
      ; 0x03c0
      ; 0x25e5
      ; 0x258c
      ; 0x2580
      ; 0x2587
      ; 0x005f
      ; 0x2581
      ; 0x258e
      ; 0x2595
      ; 0x258a
      ; 0x251c
      ; 0x2523
      ; 0x259b
      ; 0x2514
      ; 0x2517
      ; 0x2510
      ; 0x2513
      ; 0x2582
      ; 0x250c
      ; 0x250f
      ; 0x2534
      ; 0x253b
      ; 0x252c
      ; 0x2533
      ; 0x2524
      ; 0x252b
      ; 0x258b
      ; 0x2586
      ; 0x2585
      ; 0x2596
      ; 0x259d
      ; 0x251b
      ; 0x2518
      ; 0x2598
      ; 0x259f
      ; 0x259e
    ] in
  List.iter (fun c ->
      m := maybe_add (Uchar.of_int c) !m
    ) known_uchars;
  !m
