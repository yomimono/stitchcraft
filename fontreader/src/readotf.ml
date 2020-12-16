let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let debug =
  let doc = "debug output" in
  Cmdliner.Arg.(value & flag & info ["d"; "debug"] ~doc)

let info =
  let doc = "ingest otf files" in
  Cmdliner.Term.info "readotf" ~doc

module Otfreader = Fontreader.Readfiles.Reader(Fontreader.Otf2stitchfont)

let read_t = Cmdliner.Term.(const Otfreader.read $ debug $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
