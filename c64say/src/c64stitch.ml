open Stitchy.Types
(* take a phrase, output a stitch file *)

let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])
let interline =
  let doc = "extra space to insert between lines (in stitches)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output =
  let doc = "file to output json to. -, the default, is stdout" in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

(* TODO: background and text color ought to be choosable from the known C64 colors. *)
let textcolor =
  let doc = "color of text" in
  Cmdliner.Arg.(value & opt (enum C64say.Colors.cmdliner_enum) C64say.Colors.Black & info ["t"; "textcolor"] ~doc)

let bgcolor =
  let doc = "color of background" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

(* TODO: grid size should be choosable from the known valid values. *)
let grid_converter : (string * Stitchy.Types.grid) list = [
  "14", Fourteen;
  "16", Sixteen;
  "18", Eighteen;
]

let gridsize =
  let doc = "size of aida cloth grid" in
  Cmdliner.Arg.(value & opt (enum grid_converter) Stitchy.Types.Fourteen & info ["g"; "gridsize"] ~doc)

let font_name =
  let doc = "font to use (should match a database name)" in
  Cmdliner.Arg.(value & opt string "c64" & info ["f"; "font"] ~doc)

let db =
  let doc = "filename containing a sqlite database of font information" in
  Cmdliner.Arg.(value & opt file "/home/user/.stitchy/fonts.sqlite3" & info ["d"; "db"] ~doc)

let stitch _font db textcolor background gridsize phrase interline output =
  let open Lwt.Infix in
  C64say.Chars.map db >>= fun map ->
  let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
  let state = C64say.Assemble.stitch lookup textcolor background gridsize phrase interline in
  let json = Stitchy.Types.state_to_yojson state in
  Lwt.return @@ Files.stdout_or_file json output

let stitch_t = Cmdliner.Term.(const stitch $ font_name $ db $ textcolor $ bgcolor $ gridsize $ phrase $ interline $ output)

let info =
  let doc = "make a stitch file repesenting a phrase in a known font" in
  Cmdliner.Term.info "c64stitch" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (stitch_t, info)
