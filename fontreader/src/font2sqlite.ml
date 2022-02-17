let fontformat = Cmdliner.Arg.enum ["otf", `Otf;
                                    "psf", `Psf;
                                    "yaff", `Yaff;
                                   ]

let debug =
  let doc = "print debug output on stdout" in
  Cmdliner.Arg.(value & flag & info ["debug"; "d"] ~doc ~docv:"VERBOSE")

let db =
  let doc = "sqlite database location on the filesystem" in
  Cmdliner.Arg.(value & opt string "fonts.sqlite3" & info ["db"] ~doc ~docv:"DB")

let src =
  let doc = "source font" in
  Cmdliner.Arg.(value & pos 0 file "fonts" & info [] ~doc ~docv:"SRC")

let fmt =
  let doc = "type of font parser to use" in
  Cmdliner.Arg.(value & opt fontformat `Otf & info ["format"] ~doc ~docv:"FORMAT")

let font_name =
  let doc = "name by which to refer to this font" in
  Cmdliner.Arg.(value & opt string "c64" & info ["n"; "name"] ~doc ~docv:"FONT_NAME")

let ingest fmt db src font_name debug =
  let open Fontreader in
  match fmt with
  | `Otf ->
    let module Populator = Sqlite.Populate(Otf2stitchfont) in
    Populator.populate db src font_name debug
  | `Psf ->
    let module Populator = Sqlite.Populate(Psf2stitchfont) in
    Populator.populate db src font_name debug
  | `Yaff ->
    let module Populator = Sqlite.Populate(Yaff2stitchfont) in
    Populator.populate db src font_name debug

let ingest_t = Cmdliner.Term.(const ingest $ fmt $ db $ src $ font_name $ debug)

let info = Cmdliner.Term.info "populate a sqlite database with font information"

let () =
  Cmdliner.Term.eval (ingest_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
