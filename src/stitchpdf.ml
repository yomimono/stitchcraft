open Cmdliner
open Stitchcraft.Stitchpdf.Output_pdf

let grid_size =
  let doc = "Size of a grid entry representing one stitch, in points. 72 points is one inch." in
  Arg.(value & opt int 10 & info ["pixel-size"; "p"] ~docv:"PIXEL_SIZE" ~doc)

let fat_line_interval =
  let doc = "Interval at which to draw a thicker guideline on the grid and label the axis with intermediate values." in
  Arg.(value & opt int 5 & info ["guideline"; "g"] ~docv:"GUIDELINE" ~doc)

let src =
  let doc = "Stitch pattern to put in PDF. By default, will read from stdin." in
  Arg.(value & opt string "-" & info ["i"; "input"] ~docv:"INPUT" ~doc)

let dst =
  let doc = "Filename for output PDF." in
  Arg.(value & opt string "pattern.pdf" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let watermark =
  let doc = "Additional text for footer." in
  Arg.(value & opt string "" & info ["w"; "watermark"] ~docv:"WATERMARK" ~doc)

let info =
  let doc = "output a PDF for a cross-stitch pattern" in
  Cmd.info "pattern" ~doc

let paper_size =
  let sizes = [
    "a4", Pdfpaper.a4;
    "letter", Pdfpaper.usletter;
    "legal", Pdfpaper.uslegal;
  ] in
  let doc = "size of paper to use" in
  Arg.(value & opt (enum sizes) Pdfpaper.usletter & info ["paper"] ~docv:"PAPER" ~doc)

let write_pattern paper_size watermark pixel_size fat_line_interval src dst =
  let json = function
    | s when 0 = String.compare s "-" -> begin
        try Yojson.Safe.from_channel stdin with _exn -> failwith "couldn't understand input"
      end
    | src ->
      try Yojson.Safe.from_file src with _exn -> failwith "couldn't read file"
  in
  match Stitchy.Types.pattern_of_yojson (json src) with
  | Error e -> failwith @@ Printf.sprintf "couldn't parse input file: %s" e
  | Ok pattern ->
    let font_size = pixel_size - 2 in
    let symbol_map = snd @@ assign_symbols pattern.Stitchy.Types.layers in
    let cover = Stitchpdf.Coverpage.coverpage paper_size pattern in
    let symbols = symbolpage ~font_size:12 paper_size symbol_map in
    let pages = cover :: symbols :: (pages ~font_size paper_size watermark ~pixel_size ~fat_line_interval symbol_map pattern) in
    let pdf, pageroot = Pdfpage.add_pagetree pages (Pdf.empty ()) in
    let pdf = Pdfpage.add_root pageroot [] pdf in
    Pdfwrite.pdf_to_file pdf dst

let embed_t = Term.(const write_pattern $ paper_size $ watermark $ grid_size
                    $ fat_line_interval $ src $ dst)

let () = exit @@ Cmd.eval @@ Cmd.v info embed_t
