open Stitchy.Types
(* take a phrase, output a stitch file *)

let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])
let interline =
  let doc = "extra space to insert between lines (in stitches)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output =
  let doc = "file to output json to, or - for stdout" in
  Cmdliner.Arg.(value & opt string "stitch.json" & info ["o"; "output"] ~doc)

(* TODO: background and text color ought to be choosable from the known C64 colors. *)
let textcolor =
  let doc = "color of text" in
  Cmdliner.Arg.(value & opt (enum C64say.Colors.cmdliner_enum) C64say.Colors.Black & info ["t"; "textcolor"] ~doc)

(* TODO: grid size should be choosable from the known valid values. *)
let grid_converter : (string * Stitchy.Types.grid) list = [
  "14", Fourteen;
  "16", Sixteen;
  "18", Eighteen;
]

let gridsize =
  let doc = "size of aida cloth grid" in
  Cmdliner.Arg.(value & opt (enum grid_converter) Stitchy.Types.Fourteen & info ["g"; "gridsize"] ~doc)

let spoo json output =
  if 0 = String.compare output "-" then
    Yojson.Safe.to_channel stdout json
  else Yojson.Safe.to_file output json

let stitch textcolor gridsize phrase interline output =
  let state = C64say.Assemble.stitch textcolor gridsize phrase interline in
  let json = Stitchy.Types.state_to_yojson state in
  spoo json output

let stitch_t = Cmdliner.Term.(const stitch $ textcolor $ gridsize $ phrase $ interline $ output)

let info =
  let doc = "make a stitch file repesenting a phrase in c64 font" in
  Cmdliner.Term.info "c64stitch" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (stitch_t, info)
