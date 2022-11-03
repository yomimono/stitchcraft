open Stitchy.Types
open Lwt.Infix

let uchars phrases = Textstitch.uchars_of_phrase (String.concat "\n" phrases)

let find_font db phrases =
  Lwt_main.run @@ (
    let uri = Db.CLI.uri_of_db db in
    let uchars = uchars phrases |> List.sort_uniq Uchar.compare in
    Caqti_lwt.connect uri >>= function
    | Error e -> Format.eprintf "DB connection error: %a\n%!" Caqti_error.pp e; exit 1
    | Ok m ->
      let module Caqti_db = (val m) in
      Lwt_list.filter_map_s (fun u ->
          Caqti_db.collect_list Db.ORM.Fonts.contains_glyph (Uchar.to_int u) >|= function
          | Error e -> Format.eprintf "query error: %a\n%!" Caqti_error.pp e; exit 1
          | Ok [] -> Some (u, [])
          | Ok l ->
            Some (u, l)
        ) uchars >>= fun assoc_list ->
      let worst_first = List.sort (fun (_uchar, a) (_uchar, b) ->
          compare (List.length a) (List.length b)
        ) assoc_list in
      Lwt_list.iter_s (fun (uchar, l) ->
          Format.printf "char 0x%x : %d fonts\n%!" (Uchar.to_int uchar) (List.length l);
          Format.printf "%a@." Fmt.(list ~sep:sp @@ hbox @@ string) l;
          Lwt.return_unit
        ) worst_first
  )


let make_pattern font db textcolor background gridsize phrases interline output =
  let uri = Db.CLI.uri_of_db db in
  let uchars = uchars phrases in
  Caqti_lwt.connect uri >>= function
  | Error e -> Format.eprintf "DB connection error: %a\n%!" Caqti_error.pp e;
    exit 1
  | Ok m ->
    let module Caqti_db = (val m) in
    let add_glyph (u, width, height, json) map =
      match Stitchy.Types.CoordinateSet.of_yojson @@ Yojson.Safe.from_string json with
      | Error _ -> map
      | Ok stitches ->
        let uchar = Uchar.of_int u in
        let glyph = Stitchy.Types.({width; height; stitches}) in
        UcharMap.add uchar glyph map
    in
    let to_lookup = List.sort_uniq Uchar.compare (Textstitch.default_char::uchars) in
    let params = List.map (fun u -> (font, (Uchar.to_int u))) to_lookup in
    Lwt_list.fold_left_s (fun map param ->
        Caqti_db.find_opt Db.ORM.Glyphs.query param >>= function
        | Ok (Some v) -> Lwt.return @@ add_glyph v map
        | Ok None ->
          Format.eprintf "Font %s has no glyph representing 0x%x\n%!" (fst param) (snd param);
          exit 1
        | Error e -> Format.eprintf "DB lookup error: %a\n%!" Caqti_error.pp e;
          exit 1
      ) UcharMap.empty params >>= fun map ->
    let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
    let pattern = Textstitch.stitch lookup textcolor background gridsize uchars interline in
    let json = Stitchy.Types.pattern_to_yojson pattern in
    Lwt.return @@ Stitchy.Files.stdout_or_file json output

let stitch font db textcolor background gridsize phrase interline output =
  Lwt_main.run @@
    (make_pattern font db textcolor background gridsize phrase interline output
     >>= function
     | Error e -> 
       Format.eprintf "%s\n%!" e; exit 1
     | Ok () -> Lwt.return_unit
    )
