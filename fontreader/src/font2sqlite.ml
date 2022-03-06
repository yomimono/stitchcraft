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

let go fmt db src font_name debug =
  match ingest fmt db src font_name debug with
  | Error (`Msg s) -> Format.eprintf "%s\n%!" s; Error s
  | Ok l -> Ok l

let ingest_t = Cmdliner.Term.(const go $ fmt $ db $ src $ font_name $ debug)

let info = Cmdliner.Cmd.info "populate a sqlite database with font information"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info ingest_t
