let aux fmt db src font_name debug =
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

let ingest fmt db src font_name debug =
  match aux fmt db src font_name debug with
  | Error (`Msg s) -> Format.eprintf "%s\n%!" s; exit 1
  | Error (#Caqti_error.t as e) ->
    let s = Format.asprintf "%a" Caqti_error.pp e in
    Format.eprintf "%s\n%!" s; exit 1
  | Ok _ -> ()
