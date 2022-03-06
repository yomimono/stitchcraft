let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let debug =
  let doc = "debug output" in
  Cmdliner.Arg.(value & flag & info ["d"; "debug"] ~doc)

let info =
  let doc = "ingest otf files" in
  Cmdliner.Cmd.info "readotf" ~doc

module Otfreader = Fontreader.Readfiles.Reader(Fontreader.Otf2stitchfont)

let mapped debug input = Otfreader.read debug input |> function
  | Error (`Msg e) ->
    Format.eprintf "%s\n%!" e;
    Error e
  | Ok l -> Ok l

let read_t = Cmdliner.Term.(const mapped $ debug $ input)

let () = exit @@
  Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info read_t
