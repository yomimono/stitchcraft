open Stitchy.Types

let make_substrate background grid ~width ~height =
  { background;
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let max_dimensions (x1, y1) (x2, y2) =
        (max x1 x2), (max y1 y2)

let biggest_dims (layer : Stitchy.Types.layer) : (int * int) =
  (* TODO: now that stitches are a set, we could probably use the ordering
   * to do something smarter than folding over all the elements. *)
  CoordinateSet.fold max_dimensions layer.stitches (0, 0)

let find_size layers =
        List.fold_left (fun acc layer ->
      max_dimensions acc (biggest_dims layer)
    ) (0, 0) layers

(* humans usually like to supply "width, height" instead of
   "max_x", "max_y", so accommodate them preferentially. *)
let stitch background width height gridsize (layers : Stitchy.Types.layer list)=
  let max_x, max_y = find_size layers in
  let width = max (max_x + 1) width
  and height = max (max_y + 1) height
  in
  let substrate = make_substrate background gridsize ~width ~height in
  (* TODO: this is pokey enough that we should probably allow the user to bypass it *)
  let layers = List.fold_left (fun merged layer -> Stitchy.Layers.merge_threads merged [layer]) [] layers in
  {layers; substrate;}
