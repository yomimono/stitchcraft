open Stitchy.Types
open Lwt.Infix

let make_pattern font db textcolor background gridsize phrases interline output =
  let uchars = Textstitch__Assemble.uchars_of_phrase (String.concat "\n" phrases) in
  let uri = Db.CLI.uri_of_db db in
  Caqti_lwt.connect uri >>= function
  | Error e -> Format.eprintf "DB connection error: %a\n%!" Caqti_error.pp e;
    Lwt.return (Ok ())
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
    let to_lookup = List.sort_uniq Uchar.compare (Textstitch__Assemble.default_char::uchars) in
    let params = List.map (fun u -> (font, (Uchar.to_int u))) to_lookup in
    Lwt_list.fold_left_s (fun map param ->
        Caqti_db.find Db.ORM.Glyphs.query param >>= function
        | Ok v -> Lwt.return @@ add_glyph v map
        | Error e -> Format.eprintf "DB lookup error: %a\n%!" Caqti_error.pp e;
          Lwt.return map
      ) UcharMap.empty params >>= fun map ->
    let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
    let pattern = Textstitch__Assemble.stitch lookup textcolor background gridsize uchars interline in
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
