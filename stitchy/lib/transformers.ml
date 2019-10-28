open Stitchy.Types

let completely_opaque = 255

let thread_printer = Stitchy.DMC.Thread.to_rgb

let set_pixmaps blocks pixmap =
  BlockMap.iter (fun (x, y) pix ->
      let (r, g, b) = thread_printer pix.thread in
      Image.write_rgba pixmap x y r g b completely_opaque) blocks

let state_to_image state : Image.image =
  let empty_channel = Image.Pixmap.create8 in
  let width = state.substrate.max_x + 1
  and height = state.substrate.max_y + 1
  in
  let pixels = Image.(RGBA (
        empty_channel width height,
        empty_channel width height,
        empty_channel width height,
        empty_channel width height))
  in
  let image = 
  Image.{ width;
    height;
    max_val = completely_opaque; (* correlated to the color depth in some way *)
    pixels;
  } in
  set_pixmaps state.stitches image;
  image
