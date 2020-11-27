let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let info =
  let doc = "ingest psf (pc screen font) version 2 files" in
  Cmdliner.Term.info "readpsf" ~doc

(* you can tell somebody writes Java in her day job these days *)
module Psfreader = Fontreader.Readfiles.Reader(Fontreader.Psf2stitchfont)

let read_t = Cmdliner.Term.(const Psfreader.read $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
