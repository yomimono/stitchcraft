open Lwt.Infix

let db =
  let doc = "sqlite database location on the filesystem" in
  Cmdliner.Arg.(value & pos 0 file "fonts.sqlite" & info [] ~doc ~docv:"DB")

let src_dir =
  let doc = "source layer information directory" in
  Cmdliner.Arg.(value & opt dir "fonts" & info ["s"; "src"] ~doc ~docv:"SRC_DIR")

let font_name =
  let doc = "font name for glyphs in this dir" in
  Cmdliner.Arg.(value & opt string "c64" & info ["n"; "name"] ~doc ~docv:"FONT_NAME")

let make_db font_name =
  Caqti_request.exec
    Caqti_type.unit
    ("CREATE TABLE " ^ font_name ^
     " (uchar INTEGER NOT NULL PRIMARY KEY,
      glyph BLOB)")


let insert_item font_name =
  Caqti_request.exec
    (Caqti_type.tup2 Caqti_type.int Caqti_type.string) @@
    "INSERT INTO " ^ font_name ^ " (uchar, glyph) VALUES (?, ?)"

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
        Lwt.return (Ok ())

let populate db src font =
  let map = Chars.map src in
  match Lwt_main.run @@ write_db db font map with
  | Error s -> Printf.eprintf "%s\n%!" s; 1
  | Ok () -> 0

let populate_t = Cmdliner.Term.(const populate $ db $ src_dir $ font_name)

let info = Cmdliner.Term.info "populate a sqlite database with font information"

let () =
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (populate_t, info)
