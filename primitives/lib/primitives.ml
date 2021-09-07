open Stitchy.Types

let empty background grid w h : pattern =
  let substrate = { max_x = w - 1;
                    max_y = h - 1;
                    grid;
                    background;
                  }
  in
  { substrate; layers = []; backstitch_layers = [] }

let cross substrate thread x y : pattern =
  let layer = {thread;
               stitch = Cross Full;
               stitches = CoordinateSet.singleton (x, y)}
  in
  { substrate; layers = [layer]; backstitch_layers = [] }

let rect substrate thread x y w h : pattern =
  let add_stitch cs x y =
    CoordinateSet.add (x, y) cs
  in
  let add_row cs y xs =
    List.fold_left (fun cs x -> add_stitch cs x y) cs xs
  in
  let xs = List.init w (fun k -> x + k) in
  let ys = List.init h (fun k -> y + k) in
  let stitches = List.fold_left (fun cs y -> add_row cs y xs) CoordinateSet.empty ys in
  let layer = {thread; stitches; stitch = Cross Full} in
  { substrate; layers = [layer]; backstitch_layers = []}


