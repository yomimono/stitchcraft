let input =
  let doc = "pattern to generate a png listing image for ('-' for STDIN, the default)" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc)

let output =
  let doc = "png filename to output (note that - does NOT mean stdout)" in
  Cmdliner.Arg.(value & opt string "preview.png" & info ["o"; "output"] ~doc)

(* because Etsy explicitly doesn't support transparency in PNGs,
   so when we first create this image, say we don't want an
   alpha channel set.  The background fill will be completely
   opaque as well. *)
let alpha = 255

let make_background substrate =
  let open Stitchy.Types in
  let width = substrate.max_x + 1
  and height = substrate.max_y + 1
  and r, g, b = substrate.background in
  let image = Image.create_rgb ~alpha:false width height in
  Image.fill_rgb image r g b;
  image

let go input output =
  let open Rresult in
  Files.stdin_or_file input
  >>= Stitchy.Types.state_of_yojson
  >>= fun { Stitchy.Types.stitches; substrate } ->
  let image = make_background substrate in
  let paint_pixel (x, y) {Stitchy.Types.thread; _} =
    let (r, g, b) = Stitchy.DMC.Thread.to_rgb thread in
    Image.write_rgba image x y r g b alpha
  in
  Stitchy.Types.BlockMap.iter paint_pixel stitches;
  ImageLib_unix.writefile output image;
  Ok ()

let reword input output =
  Rresult.R.reword_error (fun str -> `Msg str) (go input output)

let info = Cmdliner.Term.info
    "generator of png preview images for cross-stitch patterns, suitable for use as listing images on Etsy"

let go_t = Cmdliner.Term.(const reword $ input $ output)

let () =
  let result = Cmdliner.Term.term_result go_t in
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (result, info)
