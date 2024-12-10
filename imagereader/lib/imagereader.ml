open Stitchy.Types

let read_bmp input =
  (* why yes, I have constructed a scenario in which I need to write a BMP parser *)

let patternfy debug image =
  (* image is a good ol' string fulla bytes *)
  if debug then Format.printf "butt";
  let _ = image in
  Ok ()
