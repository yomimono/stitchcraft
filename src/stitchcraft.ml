(*
### generation
- `textstitch` - make a pattern from a list of strings
- `assemble` - take a list of layers (e.g. from `ih`) and make a pattern
- `primitives`: `rect`, `piece`, `rotate`, `backstitch`, `empty` - simple figures or empty space

### manipulation

- `embellish` (hcat, vcat, repeat_corner, embellish_stitch`)

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

module Generation = struct

  let segment =
    let point = Cmdliner.Arg.(pair int int) in
    Cmdliner.Arg.(pair ~sep:'/' point point)

  let segments =
    let doc = "list of backstitches as pairs of sources and destinations on the line grid" in
    Cmdliner.Arg.(value & pos_all segment [] & info [] ~doc ~docv:"SEGMENTS")

  let thread =
    let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp) in
    let default = List.hd Stitchy.DMC.Thread.basic in
    let doc = "thread color for the stitches" in
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

  let width =
    let doc = "width of the empty grid, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 0 int 1 & info [] ~doc ~docv:"WIDTH")

  let height =
    let doc = "height of the empty grid, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 1 int 1 & info [] ~doc ~docv:"HEIGHT")

end

let backstitch_program _ _ _ _ = () (* TODO the actual backstitch program goes here *)
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
  Cmdliner.Cmd.v empty_info @@ Term.(const empty_program $ width $ height $ background $ gridsize)

let generators = Cmdliner.Cmd.(group (info "stitchcraft") [backstitch_cmd; empty_cmd])

let () =
  exit @@ Cmdliner.Cmd.eval generators
