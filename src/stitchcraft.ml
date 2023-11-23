open Cmdliner

(* common in/out stuff *)
let input =
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc:"Input file. Default stdin" ~docv:"INPUT")

let output =
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc:"Output file. Default stdout" ~docv:"OUTPUT")

(* arguments used by pattern generating commands *)
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

  let width =
    let doc = "width of the pattern, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 0 int 1 & info [] ~doc ~docv:"WIDTH")

  let height =
    let doc = "height of the pattern, in number of (non-) cross-stitches" in
    Cmdliner.Arg.(value & pos 1 int 1 & info [] ~doc ~docv:"HEIGHT")

  let x =
    let doc = "offset by x stitches from the left edge" in
    Cmdliner.Arg.(value & opt int 0 & info ["x"; "x_offset"] ~doc ~docv:"X")
  and y =
    let doc = "offset by y stitches from the top edge" in
    Cmdliner.Arg.(value & opt int 0 & info ["y"; "y_offset"] ~doc ~docv:"Y")
end

(* some manipulating commands need a list of files, not just one *)
module Manipulation = struct
  let files =
    let doc = "Patterns on which to operate, leftmost or topmost first." in
    Arg.(non_empty & pos_all file [] & info [] ~doc)

end

let surround_cmd =
  let border =
    let doc = "border file to surround the center with" in
    Cmdliner.Arg.(value & opt file "border.pattern" & info ["border"; "b"] ~doc ~docv:"BORDER")
  in
  let center =
    let doc = "center pattern to surround with a border" in
    Cmdliner.Arg.(value & opt file "center.pattern" & info ["center"; "c"] ~doc ~docv:"CENTER")
  in
  let info = Cmd.info "surround" in
  Cmd.v info Term.(const Surround.go $ border $ center $ output)


let listing_cmd = 
  let annotation =
    let doc = "annotate with 'KIT!' instead of default 'PDF!'" in
    Cmdliner.Arg.(value & flag & info ["k"; "kit"] ~doc)
  in
  let info = Cmdliner.Cmd.info "listing" in
  Cmdliner.Cmd.v info @@ Term.(const Listing.go $ input $ output $ annotation)

let pdf_cmd =
  let pdf_t =
    let open Exportpdf in
    Term.(const write_pattern $ paper_size $ watermark $ grid_size
          $ fat_line_interval $ src $ dst)
  in
  let info = Cmd.info "pdf" ~doc:"generate a pattern PDF" in
  Cmd.v info pdf_t

let estimate_info = Cmdliner.Cmd.info "estimate"
let estimate_cmd =
  let margin =
    let doc = "unstitched margin size in inches. For framing, at least 1 inch is recommended." in
    Cmdliner.Arg.(value & opt float 1. & info ["margin";"m"] ~doc ~docv:"MARGIN")
  in
  Cmdliner.Cmd.v estimate_info @@ Term.(const Estimate.estimate $ input $ margin)

let term_cmd =
  let info = Cmdliner.Cmd.info "terminal" ~doc:"display or explore a cross-stitch pattern in the terminal" in
  Cmdliner.Cmd.v info @@ Term.(const Stitchterm.disp $ input)

let patbrowse_cmd =
  let dir =
    let doc = "directory from which to read." in
    Cmdliner.Arg.(value & pos 0 dir "." & info [] ~doc)
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "browse") @@
  Term.(const Browse.disp $ dir)

let assemble_program = Assembler.go
let assemble_cmd =
  let open Generation in
  let info = Cmdliner.Cmd.info "assemble" in
  let width =
    let doc = "width for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
    Cmdliner.Arg.(value & opt int 0 & info ["w"; "width"] ~doc)
  and height =
    let doc = "height for the assembled pattern. If this is too small, it will be overridden with the smallest usable value." in
    Cmdliner.Arg.(value & opt int 0 & info ["h"; "height"] ~doc)
  in
  Cmdliner.Cmd.v info @@ Term.(const assemble_program $ input $ gridsize $ width $ height $ background $ exclude $ output)

let empty_program width height bg gridsize =
  let pattern = Primitives.empty bg gridsize ~width ~height in
  Yojson.Safe.to_channel stdout @@ Stitchy.Types.pattern_to_yojson pattern

let backstitch_cmd =
  let open Generation in
  let info = Cmdliner.Cmd.info "backstitch" in
  Cmdliner.Cmd.v info @@ Term.(const Backstitch.bs $ background $ gridsize $ thread $ segments)

let empty_cmd =
  let open Generation in
  let info = Cmdliner.Cmd.info "empty" in
  Cmdliner.Cmd.v info @@ Term.(const empty_program $ width $ height $ background $ gridsize)

let rect_info = Cmdliner.Cmd.info "rect"
let rect_cmd =
  let open Generation in
  Cmdliner.Cmd.v rect_info @@ Term.(const Rect.rect $ width $ height $ background $ thread $ gridsize $ x $ y)

let text_info = Cmdliner.Cmd.info "text"
let text_cmd =
  let open Generation in
  let phrase = Cmdliner.Arg.(value & pos_all string ["HELLO"; "WORLD"] & info [] ~docv:"PHRASE") in
  let strict =
    let doc = "fail if any character is not present in the source font" in
    Cmdliner.Arg.(value & flag & info ["s";"strict"] ~doc ~docv:"STRICT")
  in
  let min_width =
    let doc = "minimum width for each character" in
    Cmdliner.Arg.(value & opt int 0 & info ["min-width"] ~doc ~docv:"MIN_WIDTH")
  in
  let min_height =
    let doc = "minimum height for each character" in
    Cmdliner.Arg.(value & opt int 0 & info ["min-height"] ~doc ~docv:"MIN_HEIGHT")
  in
  let interline =
    let doc = "extra space to insert between lines (in stitches)" in
    Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)
  in
  let font_name =
    let doc = "file containing a usable font in stitchy's glyph map JSON format" in
    let env = Cmdliner.Cmd.Env.info "STITCH_FONT" ~doc in
    Cmdliner.Arg.(value & opt string "font.json" & info ~env ["f"; "font"] ~doc ~docv:"FONT")
  in
  Cmdliner.Cmd.v text_info @@ Term.(const Words.stitch $ strict $ font_name $ thread $ background $ gridsize $ phrase $ min_width $ min_height $ interline $ output)

let hcat_cmd =
  let hcat_t = Term.(const Hcat.go $ Manipulation.files $ output) in
  let info = Cmd.info "hcat" ~doc:"concatenate patterns around a vertical axis" in
  Cmd.v info hcat_t

let hflip_cmd =
  let info = Cmd.info "hflip" ~doc:"flip patterns around a vertical axis" in
  let hflip_t = Term.(const (Apply.go Stitchy.Operations.hflip) $ input) in
  Cmd.v info hflip_t

let piece_cmd =
  let open Generation in
  let info = Cmdliner.Cmd.info "piece" ~doc:"slice a piece out of an existing pattern" in
  let piece_t = Term.(const Piece.piece $ x $ y $ width $ height $ input) in
  Cmd.v info piece_t

let replace_cmd =
  let info = Cmdliner.Cmd.info "replace" ~doc:"replace a thread with another thread" in
  let default = List.hd Stitchy.DMC.Thread.basic in
  let src = Cmdliner.Arg.(value & pos 0 Generation.thread_conv default & info [] ~doc:"replace this thread" ~docv:"SRC") in
  let dst = Cmdliner.Arg.(value & pos 1 Generation.thread_conv default & info [] ~doc:"use this thread instead" ~docv:"DST") in
  let replace_t = Term.(const Replace.replace $ input $ output $ src $ dst) in
  Cmd.v info replace_t

let rotate_cmd =
  let info = Cmdliner.Cmd.info "rotate" ~doc:"rotate a pattern 90 degrees counterclockwise" in
  let rotate_t = Term.(const (Apply.go Stitchy.Operations.rotate_ccw) $ input) in
  Cmd.v info rotate_t

let vcat_info = Cmdliner.Cmd.info "vcat"
let vcat_cmd =
  let vcat_t = Term.(const Vcat.go $ Manipulation.files $ output) in
  let info = Cmd.info "vcat" ~doc:"concatenate patterns around a horizontal axis" in
  Cmd.v info vcat_t

let vflip_cmd =
  let info = Cmd.info "vflip" ~doc:"flip patterns around a horizontal axis" in
  let vflip_t = Term.(const (Apply.go Stitchy.Operations.vflip) $ input) in
  Cmd.v info vflip_t

let emborder_cmd =
  let transformation = Cmdliner.Arg.enum ["Nothing", Stitchy.Types.Nothing;
                                          "Flip", Stitchy.Types.Flip;
                                          "Turn", Stitchy.Types.Turn;
                                         ]
  in
  let doc = "corner pattern" in
  let corner = Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"CORNER") in
  let side = Cmdliner.Arg.(value & opt (some file) None & info ["side"; "s"] ~docv:"SIDE") in
  let fencepost = Cmdliner.Arg.(value & opt (some file) None & info ["fencepost"; "fp"] ~docv:"SIDE") in
  let corner_xform = Cmdliner.Arg.(value & opt transformation Stitchy.Types.Nothing & info ["corner_transformation"; "ct"] ~docv:(conv_docv transformation)) in
  let side_xform = Cmdliner.Arg.(value & opt transformation Stitchy.Types.Nothing & info ["side_transformation"; "st"]) in
  let fencepost_xform = Cmdliner.Arg.(value & opt transformation Stitchy.Types.Nothing & info ["fencepost_transformation"; "fpt"]) in
  let info = Cmd.info "emborder" in
  Cmd.v info Term.(const Emborder.go $ corner $ side $ fencepost $ corner_xform $ side_xform $ fencepost_xform $ output )

let font_cmd =
  let fontformat = Cmdliner.Arg.enum ["otf",  `Otf;
                                      "psf",  `Psf;
                                      "yaff", `Yaff;
                                      "js",   `Js;
                                     ]
  in
  let debug =
    let doc = "print debug output on stdout" in
    Cmdliner.Arg.(value & flag & info ["debug"; "d"] ~doc ~docv:"VERBOSE")
  in
  let fmt =
    let doc = "type of font parser to use" in
    Cmdliner.Arg.(value & opt fontformat `Otf & info ["format"] ~doc ~docv:"FORMAT")
  in
  (* TODO: use the filename (minus extension) if the user
   * didn't give us a font name *)
  let info = Cmd.info "font" in
  Cmd.v info Term.(const Font_of_file.read $ debug $ input $ fmt $ output)

let pat_cmd =
  let verbose = Cmdliner.Arg.(value & flag & info ["v"; "verbose"]) in
  let info = Cmd.info "pat" in
  Cmd.v info Term.(const Pat.main $ verbose $ input)

let importers = Cmdliner.Cmd.(group @@ info "import") [ emborder_cmd; font_cmd; pat_cmd; ]

let browsers = Cmdliner.Cmd.(group @@ info "browse") [ patbrowse_cmd; ]

let exporters = Cmdliner.Cmd.(group @@ info "export") [ listing_cmd ; pdf_cmd ]

let generators = Cmdliner.Cmd.(group (info "gen") [assemble_cmd; backstitch_cmd; empty_cmd; rect_cmd; text_cmd])

let manipulators = Cmdliner.Cmd.(group (info "manip") [ hcat_cmd; hflip_cmd; piece_cmd; replace_cmd; rotate_cmd; surround_cmd; vcat_cmd; vflip_cmd ])

let viewers = Cmdliner.Cmd.(group @@ info "view") [ estimate_cmd; term_cmd; ]

let categories = Cmdliner.Cmd.(group (info "stitchcraft") [generators; manipulators; importers; exporters; viewers; browsers])

let () =
  exit @@ Cmdliner.Cmd.eval categories
