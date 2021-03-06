open Lwt.Infix

(* TODO: yikes, this is a horrible way to escape this *)
let make_db font_name =
  Caqti_request.exec
    Caqti_type.unit
    ("CREATE TABLE '" ^ font_name ^
     "' (uchar INTEGER NOT NULL PRIMARY KEY,
      glyph BLOB)")


let insert_item font_name =
  Caqti_request.exec
    (Caqti_type.tup2 Caqti_type.int Caqti_type.string) @@
    "INSERT INTO '" ^ font_name ^ "' (uchar, glyph) VALUES (?, ?)"

let write_db db font_name map : (unit, string) result Lwt.t =
  Caqti_lwt.connect (Uri.of_string @@ "sqlite3://" ^ db) >>= function
  | Error e -> Lwt.return @@ Error (Format.asprintf "%a" Caqti_error.pp e)
  | Ok m ->
    let module Db = (val m) in
    Db.exec (make_db font_name) () >>= fun _ ->
    (* normally, we'd bail if this failed, but it's not a big deal if
       we're stomping on existing tables *)
      Lwt_list.iter_s (fun (uchar, glyph) ->
          let key = Uchar.to_int uchar
          and value = Stitchy.Types.glyph_to_yojson glyph |> Yojson.Safe.to_string
          in
          Db.exec (insert_item font_name) (key, value) >|= function
          | Error e -> Format.eprintf "Couldn't insert uchar %x: %a" key Caqti_error.pp e
          | Ok _ -> ()
        ) (Stitchy.Types.UcharMap.bindings map) >>= fun () ->
        Lwt.return @@ Ok ()
module Populate(Reader : Fontreader.Readfiles.INTERPRETER) = struct
  let populate db src font debug =
    match Bos.OS.File.read (Fpath.v src) with
    | Error e -> Error e
    | Ok s ->
      if debug then Format.eprintf "got a file, length %d\n%!" (String.length s);
      match Reader.glyphmap_of_buffer debug (Cstruct.of_string s) with
      | Error e -> Error (`Msg (Format.asprintf "%a" Reader.pp_error e))
      | Ok l ->
        if debug then
          Format.eprintf "%d glyphs discovered\n%!" (List.length l);
        let map =
          List.fold_left (fun (k, map) (glyph, uchars) ->
              let map = List.fold_left (fun map uchar -> Stitchy.Types.UcharMap.add uchar glyph map) map uchars in
              (k+1, map)
            ) (0, Stitchy.Types.UcharMap.empty) l
        in
        match Lwt_main.run @@ write_db db font (snd map) with
        | Error s -> Error (`Msg s)
        | Ok () -> Ok ()
end
