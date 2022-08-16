open Lwt.Infix

let insert debug (module Caqti_db : Caqti_lwt.CONNECTION) font glyphmap =
  (* if we get a (glyph * uchar list) list,
   * we need to generate the list of glyph-uchar pairings for each element,
   * then generate those for each glyph,
   * flatten the list,
   * and use that as params *)
  let (params : ((int * int) * string * string * int) list) =
    let params_per_uchar Stitchy.Types.({width; height; stitches}) uchar =
      let stitches_json = Stitchy.Types.CoordinateSet.to_yojson stitches |> Yojson.Safe.to_string in
      ((width, height), stitches_json, font, (Uchar.to_int uchar))
    in
    let params_per_glyph (glyph, uchars) = List.map (params_per_uchar glyph) uchars in
    if debug then Format.eprintf "inserting %d rows for %d glyphs\n%!"
        List.(length @@ map params_per_glyph glyphmap)
        (List.length glyphmap);
    List.(flatten @@ map params_per_glyph glyphmap)
  in
  if debug then Format.eprintf "about to insert %d rows\n%!" List.(length params);
  let print = function
    | Error e -> Format.printf "error inserting: %a" Caqti_error.pp e; Lwt.return_unit
    | Ok () -> Lwt.return_unit
  in
  Lwt_list.iter_s (fun p ->
      Caqti_db.exec Db.ORM.Fonts_Glyphs.insert p >>= print
    ) params >>= fun () -> Lwt.return (Ok ())

module Populate(Reader : Fontreader.Readfiles.INTERPRETER) = struct
  let populate db src font debug =
    match Bos.OS.File.read (Fpath.v src) with
    | Error e -> Error e
    | Ok s ->
      match Reader.glyphmap_of_buffer debug (Cstruct.of_string s) with
      | Error e -> Error (`Msg (Format.asprintf "%a" Reader.pp_error e))
      | Ok (l : (Stitchy.Types.glyph * Uchar.t list) list) ->
        Lwt_main.run (
          Caqti_lwt.connect @@ Db.CLI.uri_of_db db >>= function
          | Error _ as e -> Lwt.return e
          | Ok m ->
            let open Lwt_result.Infix in
            let module Caqti_db = (val m) in
            Caqti_db.exec Db.ORM.Glyphs.create () >>= fun () ->
            Caqti_db.exec Db.ORM.Fonts.create () >>= fun () ->
            Caqti_db.exec Db.ORM.Fonts_Glyphs.create () >>= fun () ->
            insert debug (module Caqti_db) font l
        )
end
