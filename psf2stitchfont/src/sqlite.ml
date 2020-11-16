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
    Db.exec (make_db font_name) () >>= function
      (* TODO: we need a fill-gaps mode *)
    | Error e -> Lwt.return @@ Error (Format.asprintf "%a" Caqti_error.pp e)
    | Ok _ ->
      Lwt_list.iter_s (fun (uchar, glyph) ->
          let key = Uchar.to_int uchar
          and value = Stitchy.Types.glyph_to_yojson glyph |> Yojson.Safe.to_string
          in
          Db.exec (insert_item font_name) (key, value) >|= function
          | Error e -> Format.eprintf "Couldn't insert uchar %x: %a" key Caqti_error.pp e
          | Ok _ -> ()
        ) (Stitchy.Types.UcharMap.bindings map) >>= fun () ->
        Lwt.return @@ Ok ()
module Populate(Reader : Psf2stitchfont__Readfiles.INTERPRETER) = struct
  let populate db src font debug =
    match Bos.OS.File.read (Fpath.v src) with
    | Error e -> Error e
    | Ok s ->
      if debug then Format.eprintf "got a file, length %d\n%!" (String.length s);
      match Reader.glyphmap_of_buffer (Cstruct.of_string s) with
      | Error e -> Error (`Msg (Format.asprintf "%a" Reader.pp_error e))
      | Ok (`Glyphmap (glyphs, uchars_list)) ->
        if debug then
          Format.eprintf "glyphs: %d, uchars_list: %d\n%!" (List.length glyphs) (List.length uchars_list);
        (* what? this should be a zipped list at least, that would make way more sense *)
        let map =
          List.fold_left (fun (k, map) uchars ->
              match List.nth_opt glyphs k with
              | None ->
                if debug then Format.eprintf "no %dth glyph\n%!" k;
                (k+1, map)
              | Some glyph ->
                let map = List.fold_left (fun map uchar -> Stitchy.Types.UcharMap.add uchar glyph map) map uchars in
                (k+1, map)
            ) (0, Stitchy.Types.UcharMap.empty) uchars_list
        in
        match Lwt_main.run @@ write_db db font (snd map) with
        | Error s -> Error (`Msg s)
        | Ok () -> Ok ()
end
