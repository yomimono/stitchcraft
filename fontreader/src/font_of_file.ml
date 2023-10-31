let dispatch_read debug input fmt =
  match fmt with
  | `Otf ->
    let module R = Fontreader.Readfiles.Reader(Fontreader.Otf2stitchfont) in
    R.read debug input
  | `Yaff ->
    let module R = Fontreader.Readfiles.Reader(Fontreader.Yaff2stitchfont) in
    R.read debug input
  | `Psf ->
    let module R = Fontreader.Readfiles.Reader(Fontreader.Yaff2stitchfont) in
    R.read debug input

let read debug input fmt output =
  match dispatch_read debug input fmt with
  | Error (`Msg e) -> Format.printf "error parsing: %s\n%!" e; exit 1
  | Ok glyphmap ->
    match Stitchy.Files.stdout_or_file (Stitchy.Types.font_to_yojson glyphmap) output with
    | Ok () -> ()
    | Error e -> Format.printf "error outputting: %s\n%!" e; exit 1
