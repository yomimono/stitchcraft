let db =
  let doc = "sqlite database location on the filesystem" in
  Cmdliner.Arg.(value & pos 0 file "fonts.sqlite" & info [] ~doc ~docv:"DB")

let src_dir =
  let doc = "source layer information directory" in
  Cmdliner.Arg.(value & opt dir "fonts" & info ["s"; "src"] ~doc ~docv:"SRC_DIR")

let font_name =
  let doc = "font name for glyphs in this dir" in
  Cmdliner.Arg.(value & opt string "c64" & info ["n"; "name"] ~doc ~docv:"FONT_NAME")

let make_db =
  Caqti_request.exec
    Caqti_type.string
    "CREATE TABLE ?
     (uchar INTEGER NOT NULL PRIMARY KEY,
      glyph BLOB)"

let write_db db font_name _map : (unit, string) result Lwt.t =
  let open Lwt.Infix in
  Caqti_lwt.connect (Uri.of_string @@ "sqlite3://" ^ db) >>= function
  | Error e -> Lwt.return @@ Error (Format.asprintf "%a" Caqti_error.pp e)
  | Ok m ->
    let module Db = (val m) in
    Db.exec make_db font_name >>= function
    | Error e -> Lwt.return @@ Error (Format.asprintf "%a" Caqti_error.pp e)
    | Ok _ -> Lwt.return @@ Ok ()

let populate db src font =
  let map = Chars.map src in
  match Lwt_main.run @@ write_db db font map with
  | Ok () -> 0
  | Error s ->
    Printf.eprintf "%s\n%!" s;
    1

let populate_t = Cmdliner.Term.(const populate $ db $ src_dir $ font_name)

let info = Cmdliner.Term.info "populate a sqlite database with font information"

let () =
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (populate_t, info)
