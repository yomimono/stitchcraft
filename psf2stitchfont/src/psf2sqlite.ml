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

module Populator = Sqlite.Populate(Psf2stitchfont)

let populate_t = Cmdliner.Term.(const Populator.populate $ db $ src $ font_name $ debug)

let info = Cmdliner.Term.info "populate a sqlite database with font information"

let () =
  Cmdliner.Term.eval (populate_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
