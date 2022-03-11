let set_db host port database user password = Postgres.({host; port; database; user; password })

let db_t =
  let host =
    let doc = "postgresql server hostname" in
    let env = Cmdliner.Cmd.Env.info "PGHOST" in
    Cmdliner.Arg.(value & opt string "localhost" & info ["h"; "host"] ~docv:"PGHOST" ~doc ~env)
  in
  let port =
    let doc = "postgresql server port" in
    let env = Cmdliner.Cmd.Env.info "PGPORT" in
    Cmdliner.Arg.(value & opt int 5432 & info ["p"; "port"] ~docv:"PGPORT" ~doc ~env)
  in
  let database =
    let doc = "postgresql database name" in
    let env = Cmdliner.Cmd.Env.info "PGDATABASE" in
    Cmdliner.Arg.(value & opt string "stitchcraft" & info ["db"] ~docv:"PGDATABASE" ~doc ~env)
  in
  let user =
    let doc = "postgresql user" in
    let env = Cmdliner.Cmd.Env.info "PGUSER" in
    Cmdliner.Arg.(value & opt string "stitchcraft" & info ["u"] ~docv:"PGUSER" ~doc ~env)
  in
  let password =
    let doc = "postgresql user's password" in
    let env = Cmdliner.Cmd.Env.info "PGPASSWORD" in
    Cmdliner.Arg.(value & opt string "s3kr1t" & info ["pass"] ~docv:"PGPASSWORD" ~doc ~env)
  in
  Cmdliner.Term.(const set_db $ host $ port $ database $ user $ password)

let fontformat = Cmdliner.Arg.enum ["otf", `Otf;
                                    "psf", `Psf;
                                    "yaff", `Yaff;
                                   ]

let debug =
  let doc = "print debug output on stdout" in
  Cmdliner.Arg.(value & flag & info ["debug"; "d"] ~doc ~docv:"VERBOSE")

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
    let module Populator = Postgres.Populate(Otf2stitchfont) in
    Populator.populate db src font_name debug
  | `Psf ->
    let module Populator = Postgres.Populate(Psf2stitchfont) in
    Populator.populate db src font_name debug
  | `Yaff ->
    let module Populator = Postgres.Populate(Yaff2stitchfont) in
    Populator.populate db src font_name debug

let go fmt db src font_name debug =
  match ingest fmt db src font_name debug with
  | Error (`Msg s) -> Format.eprintf "%s\n%!" s; Error s
  | Ok l -> Ok l

let ingest_t = Cmdliner.Term.(const go $ fmt $ db_t $ src $ font_name $ debug)

let info = Cmdliner.Cmd.info "populate a postgres database with font information"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info ingest_t
