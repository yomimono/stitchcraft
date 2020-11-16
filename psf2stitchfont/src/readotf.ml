let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let info =
  let doc = "ingest otf files" in
  Cmdliner.Term.info "readotf" ~doc

(* TODO: YIKES that is a horrible line *)
module Otfreader = Psf2stitchfont__Readfiles.Reader(Psf2stitchfont__Otf2stitchfont)

let read_t = Cmdliner.Term.(const Otfreader.read $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
