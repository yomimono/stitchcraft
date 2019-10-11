(* known issues:
   grid labels can easily be made not to align or display properly with large pixel sizes
   symbol placement is not great
   large images produce inordinately large PDFs (possibly only solvable with region detection/drawing, or another library which knows how to coalesce regions)
   *)

open Cmdliner
open Pattern.Output_pdf

let grid_size =
  let doc = "Size of a grid entry representing one stitch, in points. 72 points is one inch." in
  Arg.(value & opt int 10 & info ["pixel-size"; "p"] ~docv:"PIXEL_SIZE" ~doc)

let fat_line_interval =
  let doc = "Interval at which to draw a thicker guideline on the grid and label the axis with intermediate values." in
  Arg.(value & opt int 5 & info ["guideline"; "g"] ~docv:"GUIDELINE" ~doc)

let src =
  let doc = "Stitch pattern to put in PDF." in
  Arg.(value & pos 0 file "input.json" & info [] ~docv:"INPUT" ~doc)

let dst =
  let doc = "Filename for output PDF." in
  Arg.(value & opt string "pattern.pdf" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "output a PDF for a cross-stitch pattern" in
  Term.info "pattern" ~doc

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
    if x >= (snd page.x_range) then
      aux (fst page.x_range) (y+1) l
    else if y >= (snd page.y_range) then
      l
    else begin
      (* position is based on x and y relative to the range expressed on this page *)
      let (x_pos, y_pos) = find_upper_left doc (x - (fst page.x_range)) (y - (fst page.y_range)) in
      (* but color is based on the placement in the actual image, so pass those coordinates
         to the painting function *)
      let ((r, g, b), symbol) = match Stitchy.Types.BlockMap.find_opt (x, y) blockmap with 
        | None -> substrate.Stitchy.Types.background, Pattern.Palette.Symbol "\x20"
        | Some block ->
          let color = Stitchy.DMC.Thread.to_rgb block.thread in
          let symbol = match Stitchy.Types.SymbolMap.find_opt color doc.symbols with
            | None -> Pattern.Palette.Symbol "\x20"
            | Some symbol -> symbol
          in
          color, symbol
      in
      aux (x+1) y (paint_pixel ~x_pos ~y_pos ~pixel_size:(float_of_int doc.pixel_size)
                     r g b symbol @ l)
    end
  in
  aux (fst page.x_range) (fst page.y_range) []

let assign_symbols (blocks : Stitchy.Types.stitches) =
  let next = function
    | [] -> (Pattern.Palette.Symbol "\x20", [])
    | hd::tl -> (hd, tl)
  in
  let colors = Stitchy.Types.BlockMap.bindings blocks |>
               List.map snd |>
               List.map (fun block -> Stitchy.DMC.Thread.to_rgb block.Stitchy.Types.thread) |> List.sort_uniq Stitchy.RGB.compare in
  let color_map = Stitchy.Types.SymbolMap.empty in
  List.fold_left (fun (freelist, map) color ->
      let symbol, freelist = next freelist in
      (freelist, Stitchy.Types.SymbolMap.add color symbol map))
  (Pattern.Palette.symbols, color_map) colors

let pages ~pixel_size ~fat_line_interval state =
  let open Stitchy.Types in
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size
  in
  let width = state.substrate.max_x + 1 and height = state.substrate.max_y + 1 in
  let symbols = snd @@ assign_symbols state.Stitchy.Types.stitches in
  let doc = { pixel_size; fat_line_interval; symbols; } in
  let pixels = paint_pixels state.Stitchy.Types.stitches state.Stitchy.Types.substrate in
  let rec page x y n l =
    let l = make_page doc ~first_x:x ~first_y:y symbols ~width ~height n pixels :: l in
    if (x + xpp) >= width && (y + ypp) >= height then l    
    else if (y + ypp) >= height then page (x+xpp) 0 (n+1) l
    else page x (y + ypp) (n+1) l
  in
  List.rev @@ page 0 0 1 []

let write_pattern pixel_size fat_line_interval src dst =
  let json =
    try Yojson.Safe.from_file src with
    | _exn -> failwith "couldn't read file"
  in
  match Stitchy.Types.state_of_yojson json with
  | Error e -> failwith @@ Printf.sprintf "couldn't parse input file: %s" e
  | Ok pattern ->
    let pages = pages ~pixel_size ~fat_line_interval pattern in
    let pdf, pageroot = Pdfpage.add_pagetree pages (Pdf.empty ()) in
    let pdf = Pdfpage.add_root pageroot [] pdf in
    Pdfwrite.pdf_to_file pdf dst

let embed_t = Term.(const write_pattern $ grid_size $ fat_line_interval $ src $ dst)

let () = Term.exit @@ Term.eval (embed_t, info)
