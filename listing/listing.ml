open Stitchy.Types

let input =
  let doc = "pattern to generate a png listing image for ('-' for STDIN, the default)" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "png filename to output (note that - does NOT mean stdout)" in
  Cmdliner.Arg.(value & opt string "preview.png" & info ["o"; "output"] ~doc)

let annotation =
  let doc = "annotate with 'KIT!' instead of default 'PDF!'" in
  Cmdliner.Arg.(value & flag & info ["k"; "kit"] ~doc)

(* Etsy explicitly doesn't support transparency in PNGs,
   so when we first create this image, say we don't want an
   alpha channel set.  The background fill will be completely
   opaque as well. *)
let alpha = 255

(* TODO: reuse the contrast math we have for the pdf generator *)
let contrast = function
  | x, y, z when x <= 128 && y <= 128 && z <= 128 -> 255,255,255
  | _ -> 0, 0, 0 

(* extra_width or extra_height are amounts of additional border to create *)
let make_background ?(extra_width=0) ?(extra_height=0) substrate =
  let width = substrate.max_x + 1 + extra_width
  and height = substrate.max_y + 1 + extra_height
  and r, g, b = substrate.background
  in
  let image = Image.create_rgb ~alpha:false width height in
  Image.fill_rgb image r g b;
  image

(* we solve for four:three specifically because that's the ratio
 * etsy expects thumbnails to be in *)
let solve_for_four_to_three substrate =
  let open Annotate in
  let width = substrate.max_x + 1 and height = substrate.max_y + 1 in
  let ratio = (float_of_int width) /. (float_of_int height) in
  let better_height = int_of_float @@ 0.75 *. (float_of_int width)
  and better_width = int_of_float @@ 1.25 *. (float_of_int height)
  and min_height = (Annotate.glyph_height + 2) * 2 (* times 2 because it needs to be 
                                             of equal size on either side of the
                                             original image, but also needs
                                             to be large enough to contain the
                                             annotation *)
  and min_width = (Annotate.glyph_width + 2) * 2
  in
  if ratio <= 1.1 && (better_width - glyph_width) > width then
    `Widen (max (better_width - width) min_width)
  else if ratio >= 1.5 && (better_height - glyph_height) > height then
    `Heighten (max (better_height - height) min_height)
  (* for cases that are already just about right, we still need to make sure
   * we have enough room to annotate them *)
  else `Widen (max (better_width - width) min_width)

let paint_pixel image adjustment thread (x, y) =
  let (r, g, b) = Stitchy.DMC.Thread.to_rgb thread in
  match adjustment with
  | `Widen extra_width ->
    Image.write_rgba image (x + (extra_width / 2)) y r g b alpha
  | `Heighten extra_height ->
    Image.write_rgba image x (y + (extra_height / 2)) r g b alpha

let paint_line _image _adjustment _thread (src_x, src_y) =
  let _ = (src_x, src_y) in
  ()

let paint_layer image adjustment ({stitches; thread; _ } : layer) =
  (* TODO: we currently treat all stitches as Full Cross; we shouldn't *)
  CoordinateSet.iter (fun pixel -> paint_pixel image adjustment thread pixel) stitches

let paint_backstitch_layer image adjustment ({stitches; thread; } : backstitch_layer) =
  SegmentSet.iter (fun stitch -> paint_line image adjustment thread stitch) stitches

let go input output is_kit =
  let open Rresult in
  Stitchy.Files.stdin_or_file input
  >>= pattern_of_yojson
  >>= fun { layers; substrate; backstitch_layers; } ->
  (* TODO: painting the backstitches might
   * prompt us to adjust the size of the image we produce *)
  let adjustment = solve_for_four_to_three substrate in
  let image = match adjustment with
    | `Widen extra_width -> make_background ~extra_width substrate
    | `Heighten extra_height -> make_background ~extra_height substrate
  in
  let text_color = substrate.background in
  let orientation = match adjustment with
    | `Widen margin ->
      let left_edge = margin / 2 in
      let right_edge = left_edge + substrate.max_x + 1 in
      `Vertical (left_edge, right_edge)
    | `Heighten margin ->
      let top_edge = margin / 2 in
      let bottom_edge = top_edge + substrate.max_y + 1 in
      `Horizontal (top_edge, bottom_edge)
  in
  let annotation = if is_kit then `Kit else `Pdf in
  Annotate.add_pdf image ~annotation ~orientation ~text_color ~matting_color:(contrast text_color);
  List.iter (paint_layer image adjustment) layers;
  List.iter (paint_backstitch_layer image adjustment) backstitch_layers;
  let () = ImageLib_unix.writefile output image in
  Ok ()

let info = Cmdliner.Cmd.info "listing"

let go_t = Cmdliner.Term.(const go $ input $ output $ annotation)

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info go_t
