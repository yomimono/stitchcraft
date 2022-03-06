let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let info =
  let doc = "ingest psf (pc screen font) version 2 files" in
  Cmdliner.Cmd.info "readpsf" ~doc

module Psfreader = Fontreader.Readfiles.Reader(Fontreader.Psf2stitchfont)

let mapped debug input = Psfreader.read debug input |> function
  | Error (`Msg e) -> Format.eprintf "%s\n%!" e; Error e
  | Ok l -> Ok l

let read_t = Cmdliner.Term.(const mapped $ const false $ input)

let () = exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info read_t
