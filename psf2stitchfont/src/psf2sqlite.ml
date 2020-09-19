open Lwt.Infix

let debug =
  let doc = "print debug output on stdout" in
  Cmdliner.Arg.(value & flag & info ["debug"; "d"] ~doc ~docv:"VERBOSE")

let db =
  let doc = "sqlite database location on the filesystem" in
  Cmdliner.Arg.(value & opt string "fonts.sqlite3" & info ["db"] ~doc ~docv:"DB")

let src =
  let doc = "source PSF version 2 file (with unicode table)" in
  Cmdliner.Arg.(value & pos 0 file "fonts" & info [] ~doc ~docv:"SRC")

let font_name =
  let doc = "name by which to refer to this font" in
  Cmdliner.Arg.(value & opt string "c64" & info ["n"; "name"] ~doc ~docv:"FONT_NAME")

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

let populate db src font debug =
  match Bos.OS.File.read (Fpath.v src) with
  | Error e -> Error e
  | Ok s ->
    match Psf2stitchfont.glyphmap_of_psf_header (Cstruct.of_string s) with
    | Error e -> Error (`Msg (Format.asprintf "%a" Psf2stitchfont.pp_error e))
    | Ok (`Glyphmap (glyphs, uchars_list)) ->
      if debug then
        Format.printf "glyphs: %d, uchars_list: %d\n%!" (List.length glyphs) (List.length uchars_list);
      let map =
        List.fold_left (fun (k, map) uchars ->
            match List.nth_opt glyphs k with
            | None -> (k+1, map)
            | Some glyph ->
              let map = List.fold_left (fun map uchar -> Stitchy.Types.UcharMap.add uchar glyph map) map uchars in
              (k+1, map)
          ) (0, Stitchy.Types.UcharMap.empty) uchars_list
      in
      match Lwt_main.run @@ write_db db font (snd map) with
      | Error s -> Error (`Msg s)
      | Ok () -> Ok ()

let populate_t = Cmdliner.Term.(const populate $ db $ src $ font_name $ debug)

let info = Cmdliner.Term.info "populate a sqlite database with font information"

let () =
  Cmdliner.Term.eval (populate_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
