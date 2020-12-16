let all_chars font_name =
  Caqti_request.collect Caqti_type.unit (Caqti_type.tup2 Caqti_type.int Caqti_type.string) @@
  "SELECT * FROM `" ^ font_name ^ "`"

let load_all_chars db_module font =
  let open Lwt.Infix in
  let module Db = (val db_module : Caqti_lwt.CONNECTION) in
  Db.collect_list (all_chars font) () >|= function
  | Error e -> Error (Format.asprintf "Error looking up %s: %a" font Caqti_error.pp e)
  | Ok [] -> Error (Format.asprintf "No data for font %s" font)
  | Ok rows ->
    let m = List.fold_left (fun map (codepoint, glyph_data) ->
        match
            (Yojson.Safe.from_string glyph_data |> Stitchy.Types.glyph_of_yojson) with
        | Ok glyph ->
          Stitchy.Types.UcharMap.add (Uchar.of_int codepoint) glyph map
        | Error _ | exception _ ->
            Format.eprintf "error deserializing the glyph at code point %d (%x)" codepoint codepoint;
            map
      ) Stitchy.Types.UcharMap.empty rows
    in
    Ok m

let map db_module font =
  load_all_chars db_module font
