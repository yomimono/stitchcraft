open Stitchy.Types

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

(* extra_width or extra_height are amounts of additional border to create *)
let make_background ?(extra_width=0) ?(extra_height=0) substrate =
  let width = substrate.max_x + 1 + extra_width
  and height = substrate.max_y + 1 + extra_height
  and r, g, b = substrate.background in
  let image = Image.create_rgb ~alpha:false width height in
  Image.fill_rgb image r g b;
  image

let solve_for_four_to_three substrate =
  let width = substrate.max_x + 1 and height = substrate.max_y + 1 in
  let ratio = (float_of_int width) /. (float_of_int height) in
  let better_height = int_of_float @@ 0.75 *. (float_of_int width)
  and better_width = int_of_float @@ 1.25 *. (float_of_int height )
  in
  if ratio <= 1.1 && (better_width - 2) > width then
    `Widen (better_width - width)
  else if ratio >= 1.5 && (better_height - 2) > height then
    `Heighten (better_height - height)
  else
    `Just_right ((better_width - width), (better_height - height))

let paint_pixel image adjustment (x, y) {thread; _} =
  let (r, g, b) = Stitchy.DMC.Thread.to_rgb thread in
  match adjustment with
  | `Widen extra_width ->
    Image.write_rgba image (x + (extra_width / 2)) y r g b alpha
  | `Heighten extra_height ->
    Image.write_rgba image x (y + (extra_height / 2)) r g b alpha
  | _ ->
    Image.write_rgba image x y r g b alpha

let go input output =
  let open Rresult in
  Files.stdin_or_file input
  >>= state_of_yojson
  >>= fun { stitches; substrate } ->
  let adjustment = solve_for_four_to_three substrate in
  let image = match adjustment with
    | `Widen extra_width -> make_background ~extra_width substrate
    | `Heighten extra_height -> make_background ~extra_height substrate
    | `Just_right _ -> make_background substrate
  in
  Stitchy.Types.BlockMap.iter (paint_pixel image adjustment) stitches;
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
