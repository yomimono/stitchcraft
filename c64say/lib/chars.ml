module CharMap = Map.Make(Uchar)

(* some useful dimensions to have access to from a phrase *)
type dimensions = {
  height : int;
  width : int;
}

let h, w = 8, 8 (* c64 chars are 8x8 *)

(* TODO: this is silly; we should do one pass to write the text and base
   the size of the substrate on that information, rather than
   trying to duplicate the logic correctly *)
let get_dimensions phrase interline =
  let decoder = Uutf.decoder (`String phrase) in
  let figure_height_and_width decoder =
    let rec line_geometry decoder (completed_lengths, line_length_so_far) =
      match Uutf.decode decoder with
      | `Malformed _ | `Await -> line_geometry decoder (completed_lengths, line_length_so_far)
      | `End -> (line_length_so_far :: completed_lengths, 0)
      | `Uchar u ->
        match Uucp.Gc.general_category u with
        | `Zl | `Cc when Uchar.to_char u = '\n' ->
          line_geometry decoder (line_length_so_far :: completed_lengths, 0)
        | _ -> match Uucp.Break.tty_width_hint u with
          | n when n <= 0 -> line_geometry decoder (completed_lengths, line_length_so_far)
          | n -> line_geometry decoder (completed_lengths, (line_length_so_far + n))
    in
    fst @@ line_geometry decoder ([], 0)
  in
  let lines = figure_height_and_width decoder in
  let height = (List.length lines) * h in
  let interline = (max 0 (List.length lines - 1)) * interline in
  let longest_line = List.fold_left (fun longest length -> max longest length) 0 lines in
  let width = longest_line * w in
  { height = height + interline;
    width }

(* TODO: this does the wrong thing for '-' and probably other chars
   which are at a different address than they are in ASCII,
   despite the best attempts of is_upper/is_lower *)
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
      | 0x2500 | 0x2501 -> "a0"
      | 0x2660 -> "c1"
      | 0x2502 | 0x007c | 0x2503 -> "c2"
      | 0x256e -> "c9"
      | 0x2570 -> "ca"
      | 0x256f -> "cb"
      | 0x2572 -> "cd"
      | 0x2571 -> "ce"
      | 0x2022 | 0x00b7 | 0x0025cf | 0x2219 -> "d1"
      | 0x2665 -> "d3"
      | 0x256d -> "d5"
      | 0x2573 -> "d6"
      | 0x25cb | 0x2218 | 0x25e6 | 0x25ef -> "d7"
      | 0x2663 -> "d8"
      | 0x2666 | 0x25c6 -> "da"
      | 0x253c | 0x254b -> "db"
      | 0x03c0 -> "de"
      | 0x25e5 -> "df"
      | 0x258c -> "a1"
      | 0x2584 -> "a2"
      | 0x2594 -> "a3"
      | 0x005f | 0x2581 -> "a4"
      | 0x258e -> "a5"
      | 0x2595 -> "a6"
      | 0x258a -> "a7"
      | 0x251c | 0x2523 -> "ab"
      | 0x259b -> "ac"
      | 0x2514 | 0x2517 -> "ad"
      | 0x2510 | 0x2513 -> "ae"
      | 0x2582 -> "af"
      | 0x250c | 0x250f -> "b0"
      | 0x2534 | 0x253b -> "b1"
      | 0x252c | 0x2533 -> "b2"
      | 0x2524 | 0x252b -> "b3"
      | 0x258d -> "b5"
      | 0x2583 -> "b9"
      | 0x2596 -> "bb"
      | 0x259d -> "bc"
      | 0x251b | 0x2518 -> "bd"
      | 0x2598 | 0x259f -> "be"
      | 0x259e -> "bf"
      | _ -> ""
    in
    Printf.sprintf "%s/%s.png.layer0" base byte

let load_layer file =
  try Ok (Yojson.Safe.from_file file |> Stitchy.Types.glyph_of_yojson)
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
