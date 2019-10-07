(* known issues:
   grid labels can easily be made not to align or display properly with large pixel sizes
   symbol placement is not great
   large images produce inordinately large PDFs (possibly only solvable with region detection/drawing, or another library which knows how to coalesce regions)
   *)

open Cmdliner
open Output_pdf

let grid_size =
  let doc = "Size of a grid entry representing one stitch, in points. 72 points is one inch." in
  Arg.(value & opt int 10 & info ["pixel-size"; "p"] ~docv:"PIXEL_SIZE" ~doc)

let fat_line_interval =
  let doc = "Interval at which to draw a thicker guideline on the grid and label the axis with intermediate values." in
  Arg.(value & opt int 5 & info ["guideline"; "g"] ~docv:"GUIDELINE" ~doc)

let src =
  let doc = "PNG to put in PDF." in
  Arg.(value & pos 0 file "input.png" & info [] ~docv:"INPUT" ~doc)

let dst =
  let doc = "Filename for output PDF." in
  Arg.(value & opt string "pattern.pdf" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let info =
  let doc = "construct a cross-stitch pattern based on an image" in
  Term.info "pattern" ~doc

let paint_pixels image doc page =
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
      let (r, g, b) = Image.read_rgba image x y (fun r g b _ -> r, g, b) in
      let symbol = match Palette.ColorMap.find_opt (r, g, b) doc.symbols with
        | None -> Palette.Symbol "\x20"
        | Some symbol -> symbol
      in
      aux (x+1) y (paint_pixel ~x_pos ~y_pos ~pixel_size:(float_of_int doc.pixel_size)
                     r g b symbol @ l)
    end
  in
  aux (fst page.x_range) (fst page.y_range) []

let assign_symbols image =
  let next = function
    | [] -> (Palette.Symbol "\x20", [])
    | hd::tl -> (hd, tl)
  in
  (* TODO: is there a quicker way to get the palette than running through the pixmap? *)
  let color_counts = Palette.ColorMap.empty in
  let count_color freelist x y m =
    Image.read_rgba image x y (fun r g b a ->
        (* ignore entirely transparent pixels *)
        match a with
        | 0 -> (freelist, m)
        | _ ->
          begin
            let (new_freelist, new_map) =
              match Palette.ColorMap.mem (r, g, b) m with
              | true -> (freelist, m)
              | false ->
                let (symbol, new_freelist) = next freelist in
                (new_freelist, Palette.ColorMap.add (r, g, b) symbol m)
            in
            (new_freelist, new_map)
          end
      )
  in
  let rec aux freelist x y m =
    if x >= image.Image.width then
      aux freelist 0 (y+1) m
    else if y >= image.Image.height then
      (freelist, m)
    else begin
      let (freelist, m) = count_color freelist x y m in
      aux freelist (x+1) y m
    end
  in
  let (_, color_map) = aux Palette.symbols 0 0 color_counts in
  color_map

let pages ~pixel_size ~fat_line_interval image =
  let xpp = x_per_page ~pixel_size
  and ypp = y_per_page ~pixel_size
  and width, height = image.Image.width, image.Image.height in
  let symbols = assign_symbols image in
  let doc = { pixel_size; fat_line_interval; symbols; } in
  let pixels = paint_pixels image in
  let rec page x y n l =
    let l = make_page doc ~first_x:x ~first_y:y ~width ~height n pixels :: l in
    if (x + xpp) >= width && (y + ypp) >= height then l    
    else if (y + ypp) >= height then page (x+xpp) 0 (n+1) l
    else page x (y + ypp) (n+1) l
  in
  List.rev @@ page 0 0 1 []

let embed_image pixel_size fat_line_interval src dst =
  let image = ImagePNG.ReadPNG.parsefile @@ ImageUtil_unix.chunk_reader_of_path src in
  let pages = pages ~pixel_size ~fat_line_interval image in
  let pdf, pageroot = Pdfpage.add_pagetree pages (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdf in
  Pdfwrite.pdf_to_file pdf dst

let embed_t = Term.(const embed_image $ grid_size $ fat_line_interval $ src $ dst)

let () = Term.exit @@ Term.eval (embed_t, info)
