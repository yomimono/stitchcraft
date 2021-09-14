open Stitchy.Types

let input =
  let doc = "pattern to generate a png listing image for ('-' for STDIN, the default)" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "png filename to output (note that - does NOT mean stdout)" in
  Cmdliner.Arg.(value & opt string "preview.png" & info ["o"; "output"] ~doc)

(* Etsy explicitly doesn't support transparency in PNGs,
   so when we first create this image, say we don't want an
   alpha channel set.  The background fill will be completely
   opaque as well. *)
let alpha = 255

(* extra_width or extra_height are amounts of additional border to create *)
let make_background ?(extra_width=0) ?(extra_height=0) substrate =
  let width = substrate.max_x + 1 + extra_width
  and height = substrate.max_y + 1 + extra_height
  and r, g, b = substrate.background in
  let image = Image.create_rgb ~alpha:false width height in
  Image.fill_rgb image r g b;
  image

let solve_for_four_to_three substrate =
  let open Annotate in
  let width = substrate.max_x + 1 and height = substrate.max_y + 1 in
  let ratio = (float_of_int width) /. (float_of_int height) in
  let better_height = int_of_float @@ 0.75 *. (float_of_int width)
  and better_width = int_of_float @@ 1.25 *. (float_of_int height )
  in
  if ratio <= 1.1 && (better_width - (fst pdf_glyph_dims)) > width then
    `Widen (better_width - width)
  else if ratio >= 1.5 && (better_height - (snd pdf_glyph_dims)) > height then
    `Heighten (better_height - height)
  else
    `Just_right ((better_width - width), (better_height - height))

let paint_pixel image adjustment thread (x, y) =
  let (r, g, b) = Stitchy.DMC.Thread.to_rgb thread in
  match adjustment with
  | `Widen extra_width ->
    Image.write_rgba image (x + (extra_width / 2)) y r g b alpha
  | `Heighten extra_height ->
    Image.write_rgba image x (y + (extra_height / 2)) r g b alpha
  | _ ->
    Image.write_rgba image x y r g b alpha

let paint_line _image _adjustment _thread (src_x, src_y) =
  let _ = (src_x, src_y) in
  ()

let paint_layer image adjustment ({stitches; thread; _ } : layer) =
  (* TODO: we currently treat all stitches as Full Cross; we shouldn't *)
  CoordinateSet.iter (fun pixel -> paint_pixel image adjustment thread pixel) stitches

let paint_backstitch_layer image adjustment ({stitches; thread; } : backstitch_layer) =
  SegmentSet.iter (fun stitch -> paint_line image adjustment thread stitch) stitches

let go input output =
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
    | `Just_right _ ->
      let extra_width = (fst Annotate.pdf_glyph_dims) * 2
      and extra_height = (snd Annotate.pdf_glyph_dims) * 2
      in
      make_background ~extra_width ~extra_height substrate
  in
  let text_color = 0, 0, 0 in
  let orientation = match adjustment with
    | `Widen margin -> `Vertical (margin / 2)
    | `Heighten margin -> `Horizontal (margin / 2)
    | `Just_right _ -> (* arbitrarily, horizontal *)
      `Horizontal (snd Annotate.pdf_glyph_dims)
  in
  Annotate.add_pdf image ~orientation text_color;
  List.iter (paint_layer image adjustment) layers;
  List.iter (paint_backstitch_layer image adjustment) backstitch_layers;
  let () = ImageLib_unix.writefile output image in
  Ok ()

let reword input output =
  Rresult.R.reword_error (fun str -> `Msg str) (go input output)

let info = Cmdliner.Term.info
    "generator of png preview images for cross-stitch patterns, suitable for use as listing images on Etsy"

let go_t = Cmdliner.Term.(const reword $ input $ output)

let () =
  let result = Cmdliner.Term.term_result go_t in
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (result, info)
