(* some useful dimensions to have access to from a phrase *)
type dimensions = {
  height : int;
  width : int;
}

let h, w = 8, 8 (* c64 chars are 8x8 *)
let font = "c64"

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

(* TODO: don't hardcode c64, and figure out how to compartmentalize
   special logic about maps of Unicode characters? *)
let find_char =
  Caqti_request.find_opt Caqti_type.int Caqti_type.string
    "SELECT glyph FROM c64 WHERE uchar = ?"

let load_layer db_module c =
  let open Lwt.Infix in
  let module Db = (val db_module : Caqti_lwt.CONNECTION) in
  Db.find_opt find_char (Uchar.to_int c) >|= function
  | Error e -> Error (Format.asprintf "Error looking up %x: %a" (Uchar.to_int c) Caqti_error.pp e)
  | Ok None -> Error (Format.asprintf "No result for %x" (Uchar.to_int c))
  | Ok (Some s) ->
    try (Yojson.Safe.from_string s |> Stitchy.Types.glyph_of_yojson)
    with _ -> Error "JSON deserialization error or glyph reconstruction error"

let maybe_add db_module c m =
  let open Lwt.Infix in
  load_layer db_module c >|= function
  | Ok layer -> Stitchy.Types.UcharMap.add c layer m
  | Error e ->
    Printf.eprintf "%s\n%!" e;
    m

let map db_module =
  let open Lwt.Infix in
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
  let m = ref Stitchy.Types.UcharMap.empty in
  let fetch_char c = maybe_add db_module (Uchar.of_int c) !m in
  let minimum_printable = 0x20 in
  let chars = List.init (255-minimum_printable) (fun c -> minimum_printable + c) in
  (* get the characters in base ASCII *)
  Lwt_list.iter_s
    (fun c ->
       fetch_char c >>= fun new_map ->
       m := new_map;
       Lwt.return_unit
    ) chars >>= fun () ->
  (* add the known PETSCII special cases *)
  Lwt_list.iter_s (fun c ->
      maybe_add db_module (Uchar.of_int c) !m >>= fun new_map ->
      m := new_map;
      Lwt.return_unit
    ) known_uchars >>= fun () ->
  Printf.eprintf "font map has %d elements\n%!" (Stitchy.Types.UcharMap.cardinal !m);
  Lwt.return !m
