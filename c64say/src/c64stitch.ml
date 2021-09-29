open Stitchy.Types
(* take a phrase, output a stitch file *)

let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])
let interline =
  let doc = "extra space to insert between lines (in stitches)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output =
  let doc = "file to output json to. -, the default, is stdout" in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

let textcolor =
  let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp) in
  let default = List.hd Stitchy.DMC.Thread.basic in
  let doc = "thread identifier for text" in
  Cmdliner.Arg.(value & opt thread_conv default & info ["t"; "textcolor"] ~doc)

let bgcolor =
  let doc = "color of background" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

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
  let env = Cmdliner.Arg.env_var "STITCH_FONT" ~doc in
  Cmdliner.Arg.(value & opt string "Bm437_PhoenixEGA_8x8" & info ~env ["f"; "font"] ~doc ~docv:"FONT")

let db =
  let doc = "filename containing a sqlite database of font information" in
  Cmdliner.Arg.(value & opt file "/tmp/fonts.sqlite3" & info ["d"; "db"] ~doc)

let make_pattern font db textcolor background gridsize phrase interline output =
  let open Lwt.Infix in
  Caqti_lwt.connect (Uri.of_string @@ "sqlite3://" ^ db) >>= function
  | Error e -> Lwt.return @@ Error (Format.asprintf "%a" Caqti_error.pp e)
  | Ok db ->
    C64say.Chars.map db font >>= function
    | Error s -> Lwt.return (Error s)
    | Ok map ->
      let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
      let pattern = C64say.Assemble.stitch lookup textcolor background gridsize phrase interline in
      let json = Stitchy.Types.pattern_to_yojson pattern in
      Lwt.return @@ Stitchy.Files.stdout_or_file json output

let stitch font db textcolor background gridsize phrase interline output =
  match Lwt_main.run @@ make_pattern font db textcolor background gridsize phrase interline output with
  | Error e -> Printf.eprintf "%s\n%!" e; exit 1
  | Ok () -> exit 0

let stitch_t = Cmdliner.Term.(const stitch $ font_name $ db $ textcolor $ bgcolor $ gridsize $ phrase $ interline $ output)

let info =
  let doc = "make a stitch file repesenting a phrase in a known font" in
  Cmdliner.Term.info "c64stitch" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (stitch_t, info)
