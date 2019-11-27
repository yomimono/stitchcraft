open Cmdliner
open Pattern.Output_pdf

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
  Term.info "pattern" ~doc

let paper_size =
  let sizes = [ (*
    "a0", Pdfpaper.a0; "a1", Pdfpaper.a1; "a2", Pdfpaper.a2; "a3", Pdfpaper.a3;
    "a4", Pdfpaper.a4; "a5", Pdfpaper.a5; "a6", Pdfpaper.a6; "a7", Pdfpaper.a7;
    "a8", Pdfpaper.a8; "a9", Pdfpaper.a9; "a10", Pdfpaper.a10; *)
    (* a4 seems not to work, so TODO commenting it out *)
    "letter", Pdfpaper.usletter;
    "legal", Pdfpaper.uslegal;
  ] in
  let doc = "size of paper to use" in
  Arg.(value & opt (enum sizes) Pdfpaper.usletter & info ["paper"] ~docv:"PAPER" ~doc)

let get_symbol blockmap symbols substrate x y =
  match Stitchy.Types.BlockMap.find_opt (x, y) blockmap with 
  | None -> substrate.Stitchy.Types.background, Stitchy.Symbol.default
  | Some block ->
    let color = Stitchy.DMC.Thread.to_rgb block.Stitchy.Types.thread in
    let symbol = match Stitchy.Types.SymbolMap.find_opt color symbols with
      | None -> Stitchy.Symbol.default
      | Some symbol -> symbol
    in
    color, symbol

let paint_pixels (blockmap : Stitchy.Types.block Stitchy.Types.BlockMap.t) substrate doc page =
  (* x and y are the relative offsets within the page. *)
  (* (so, even if this is page 3 and the grid that's painted over this will be
     stitches 100-200 (x) and 50-70 (y), x and y will count up from 0 here.
     we will add the correct offset to get the right color when indexing into
     the image.)
     the location math is based on this page-relative counting - so, the pixel
     at the third square down and fourth to the right from the upper-left hand
     corner is at the same location regardless of whether it represents (100, 50) from
     the original image, or (50, 25), or (1024, 768). *)
  let rec aux x y l =
    (* if we've advanced beyond the horizontal edge, start over on the left edge of the next row *)
    if x >= (snd page.x_range) then aux (fst page.x_range) (y+1) l
    (* if we've advanced beyond the vertical edge, time to stop *)
    else if y >= (snd page.y_range) then l
    (* otherwise, let's paint the pixel *)
    else begin
      (* position is based on x and y relative to the range expressed on this page *)
      let (x_pos, y_pos) = find_upper_left doc (x - (fst page.x_range)) (y - (fst page.y_range)) in

      (* but color is based on the placement in the actual image, so pass those coordinates
         to the painting function *)
      let ((r, g, b), symbol) = get_symbol blockmap doc.symbols substrate x y in
      let this_pixel =
        paint_pixel ~x_pos ~y_pos
          ~pixel_size:(float_of_int doc.pixel_size)
          r g b symbol
      in
      aux (x+1) y (this_pixel @ l)
    end
  in
  aux (fst page.x_range) (fst page.y_range) []

let assign_symbols (blocks : Stitchy.Types.stitches) =
  let next = function
    | [] -> (Stitchy.Symbol.default, [])
    | hd::tl -> (hd, tl)
  in
  let colors = Stitchy.Types.BlockMap.bindings blocks |>
               List.map snd |>
               List.map (fun block -> Stitchy.DMC.Thread.to_rgb block.Stitchy.Types.thread) |> List.sort_uniq Stitchy.RGB.compare in
  let color_map = Stitchy.Types.SymbolMap.empty in
  List.fold_left (fun (freelist, map) color ->
      let symbol, freelist = next freelist in
      (freelist, Stitchy.Types.SymbolMap.add color symbol map))
  (Stitchy.Symbol.printable_symbols, color_map) colors

let pages paper_size watermark ~pixel_size ~fat_line_interval state =
  let open Stitchy.Types in
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size
  in
  let width = state.substrate.max_x + 1 and height = state.substrate.max_y + 1 in
  let symbols = snd @@ assign_symbols state.Stitchy.Types.stitches in
  let doc = { paper_size; pixel_size; fat_line_interval; symbols; } in
  let pixels = paint_pixels state.Stitchy.Types.stitches state.Stitchy.Types.substrate in
  let rec page x y n l =
    let l = make_page doc ~watermark ~first_x:x ~first_y:y symbols ~width ~height n pixels :: l in
    if (x + xpp) >= width && (y + ypp) >= height then l    
    else if (y + ypp) >= height then page (x+xpp) 0 (n+1) l
    else page x (y + ypp) (n+1) l
  in
  List.rev @@ page 0 0 1 []

let write_pattern paper_size watermark pixel_size fat_line_interval src dst =
  let json = function
    | s when 0 = String.compare s "-" -> begin
        try Yojson.Safe.from_channel stdin with _exn -> failwith "couldn't understand input"
      end
    | src ->
      try Yojson.Safe.from_file src with _exn -> failwith "couldn't read file"
  in
  match Stitchy.Types.state_of_yojson (json src) with
  | Error e -> failwith @@ Printf.sprintf "couldn't parse input file: %s" e
  | Ok pattern ->
    let cover = coverpage paper_size pattern in
    let pages = cover :: (pages paper_size watermark ~pixel_size ~fat_line_interval pattern) in
    let pdf, pageroot = Pdfpage.add_pagetree pages (Pdf.empty ()) in
    let pdf = Pdfpage.add_root pageroot [] pdf in
    Pdfwrite.pdf_to_file pdf dst

let embed_t = Term.(const write_pattern $ paper_size $ watermark $ grid_size
                    $ fat_line_interval $ src $ dst)

let () = Term.exit @@ Term.eval (embed_t, info)
