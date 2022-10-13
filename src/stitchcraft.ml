(*
### generation
- `textstitch` - make a pattern from a list of strings
- `assemble` - take a list of layers (e.g. from `ih`) and make a pattern
- `primitives`: `rect`, `backstitch`, `empty` - simple figures or empty space

### manipulation

- `embellish` (hcat, vcat, repeat_corner, embellish_stitch`)
- `piece`, `rotate`

### output

- `stitch_pattern` - patterns to pdfs
- `listing` - prepares preview images for patterns & annotates

### viewers

- `notty_canvas` - view patterns in the terminal
- `patbrowser` - browse through patterns and clip them for database
- `estimate` - estimate materials cost

### elements managers

- `patreader` - converts .pat files to stitchy
- `fontreader` - ingests raster fonts to the font database
- `ingest_pattern` - ingests patterns to the pattern database
   *)
open Cmdliner

let input =
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc:"Input file. Default stdin" ~docv:"INPUT")

let output =
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc:"Output file. Default stdout" ~docv:"OUTPUT")

module Generation = struct

  let segment =
    let point = Cmdliner.Arg.(pair int int) in
    Cmdliner.Arg.(pair ~sep:'/' point point)

  let segments =
    let doc = "list of backstitches as pairs of sources and destinations on the line grid, e.g. 0,0/1,1" in
    Cmdliner.Arg.(value & pos_all segment [] & info [] ~doc ~docv:"SEGMENTS")

  let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp)

  let thread =
    let default = List.hd Stitchy.DMC.Thread.basic in
    let doc = "thread color" in
    Cmdliner.Arg.(value & opt thread_conv default & info ["t"; "thread"] ~doc ~docv:"THREAD")

  let background =
    let doc = "background color" in
    Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

  let grid_converter : (string * Stitchy.Types.grid) list = [
    "14", Fourteen;
    "16", Sixteen;
    "18", Eighteen;
  ]

  let gridsize =
    let doc = "size of aida cloth grid" in
    Cmdliner.Arg.(value & opt (enum grid_converter) Stitchy.Types.Fourteen & info ["g"; "gridsize"] ~doc)

  let exclude =
    let doc = "thread identifiers to omit from the pattern. This is useful when consuming output from a program with no support for transparency - put stitch colors you expect to be the background here." in
    Cmdliner.Arg.(value & opt_all thread_conv [] & info ["e"; "exclude"] ~doc)

end

let assemble_program = Assembler.go
let assemble_info = Cmdliner.Cmd.info "assemble"
let assemble_cmd =
  let open Generation in
  let width =
    let doc = "width for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
    Cmdliner.Arg.(value & opt int 0 & info ["w"; "width"] ~doc)
  and height =
    let doc = "height for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
    Cmdliner.Arg.(value & opt int 0 & info ["h"; "height"] ~doc)
  in
  Cmdliner.Cmd.v assemble_info @@ Term.(const assemble_program $ input $ gridsize $ width $ height $ background $ exclude $ output)

let backstitch_program = Backstitch.bs
let empty_program width height bg gridsize =
  let pattern = Primitives.empty bg gridsize ~width ~height in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson pattern

let backstitch_info = Cmdliner.Cmd.info "backstitch"
let backstitch_cmd =
  let open Generation in
  Cmdliner.Cmd.v backstitch_info @@ Term.(const backstitch_program $ background $ gridsize $ thread $ segments)

let empty_info = Cmdliner.Cmd.info "empty"
let empty_cmd =
  let open Generation in
  let width =
    let doc = "width of the pattern, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 0 int 1 & info [] ~doc ~docv:"WIDTH")
  and height =
    let doc = "height of the pattern, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 1 int 1 & info [] ~doc ~docv:"HEIGHT")
  in
  Cmdliner.Cmd.v empty_info @@ Term.(const empty_program $ width $ height $ background $ gridsize)

let textstitch_info = Cmdliner.Cmd.info "textstitch"
let textstitch_cmd =
  let open Generation in
  let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [] ~docv:"PHRASE") in
  let interline =
    let doc = "extra space to insert between lines (in stitches)" in
    Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)
  in
  let font_name =
    let doc = "font to use (should match a database name)" in
    let env = Cmdliner.Cmd.Env.info "STITCH_FONT" ~doc in
    Cmdliner.Arg.(value & opt string "c64" & info ~env ["f"; "font"] ~doc ~docv:"FONT")
  in
  Cmdliner.Cmd.v textstitch_info @@ Term.(const Textstitch.stitch $ font_name $ Db.CLI.db_t $ thread $ background $ gridsize $ phrase $ interline $ output)

let generators = Cmdliner.Cmd.(group (info "stitchcraft") [assemble_cmd; backstitch_cmd; empty_cmd; textstitch_cmd])

let () =
  exit @@ Cmdliner.Cmd.eval generators
