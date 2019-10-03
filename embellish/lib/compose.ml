open Image

type reflection = None
                | Vertical
                | Horizontal
                | VH (* both vertically and horizontally, as in the bottom right-hand corner *)
                | Diagonal
                | DH (* both diagonally and horizontally, as on the right-hand edge *)

let max3 a b c : int = max (max a b) (max b c)

let border_repetitions ~center ~side = match center mod side with
  | 0 -> center / side
  | _ -> (center / side) + side

(* blit [src] into [dst] starting at ([dst_x_off], [dst_y_off]) *)
(* if [dst] is too small, copy what you can *)
let blit_rgbas ?(reflection=None) ~src ~dst ~dst_x_off ~dst_y_off =
  (* a pair of nested for loops? in my OCaml code?
     it's more likely than you think *)
  for x = 0 to src.width - 1 do
    for y = 0 to src.height - 1 do
      let translation = match reflection with
      | None ->
        Image.write_rgba dst (x+dst_x_off) (y+dst_y_off)
      | Vertical ->
        let bottom_edge = dst_y_off + src.height - 1 in
        Image.write_rgba dst (x+dst_x_off) (bottom_edge - y)
      | Horizontal ->
        let right_edge = dst_x_off + src.width - 1 in
        Image.write_rgba dst (right_edge - x) (y+dst_y_off)
      | VH ->
        let bottom_edge = dst_y_off + src.height - 1 in
        let right_edge = dst_x_off + src.width - 1 in
        Image.write_rgba dst (right_edge - x) (bottom_edge - y)
      | Diagonal ->
        (* xs are ys, ys are xs, what a world we live in *)
        Image.write_rgba dst (y+dst_x_off) (x+dst_y_off)
      | DH ->
        let right_edge = dst_x_off + src.height - 1 in
        Image.write_rgba dst (right_edge - y) (x+dst_y_off)
      in
      Image.read_rgba src x y translation
    done
  done

let blit_corners corner canvas =
  blit_rgbas ~reflection:None ~src:corner ~dst:canvas ~dst_x_off:0 ~dst_y_off:0;
  blit_rgbas ~reflection:Horizontal ~src:corner ~dst:canvas ~dst_x_off:(canvas.width - corner.width) ~dst_y_off:0;
  blit_rgbas ~reflection:Vertical ~src:corner ~dst:canvas ~dst_x_off:0 ~dst_y_off:(canvas.height - corner.height);
  blit_rgbas ~reflection:VH ~src:corner ~dst:canvas ~dst_x_off:(canvas.width - corner.width) ~dst_y_off:(canvas.height - corner.height)

let blit_horiz_edges corner center side canvas =
  let horiz_border_reps = border_repetitions ~center:center.width ~side:side.width in
  let rec nth_rep = function
    | n when n < 0 -> ()
    | n ->
      blit_rgbas ~reflection:None ~src:side ~dst:canvas ~dst_x_off:(corner.width+(n*side.width)) ~dst_y_off:0;
      blit_rgbas ~reflection:Vertical ~src:side ~dst:canvas ~dst_x_off:(corner.width+(n*side.width)) ~dst_y_off:(canvas.height-side.height);
      nth_rep (n-1)
  in
  nth_rep horiz_border_reps

let blit_vert_edges corner center side canvas =
  let vert_border_reps = border_repetitions ~center:center.height ~side:side.width in
  let rec nth_rep = function
    | n when n < 0 -> ()
    | n ->
      blit_rgbas ~reflection:Diagonal ~src:side ~dst:canvas ~dst_x_off:0 ~dst_y_off:(corner.height+(n*side.width));
      blit_rgbas ~reflection:DH ~src:side ~dst:canvas ~dst_x_off:(canvas.width-corner.width) ~dst_y_off:(corner.height+(n*side.width));
      nth_rep (n-1)
  in
  nth_rep vert_border_reps

let blit_center corner center side canvas =
  (* figure out how much extra whitespace we needed to make the borders work *)
  let horiz_border_reps = border_repetitions ~center:center.width ~side:side.width in
  let vert_border_reps = border_repetitions ~center:center.height ~side:side.width in
  let extra_x = (horiz_border_reps * side.width) - center.width in
  let extra_y = (vert_border_reps * side.width) - center.height in
  blit_rgbas ~reflection:None ~src:center ~dst:canvas ~dst_x_off:(corner.width + extra_x/2) ~dst_y_off:(corner.height + extra_y/2)

let compose (corner : Image.image) (side : Image.image) (center : Image.image) : Image.image =
  (* probably this needs to be a result type; perhaps the resulting image would be too large, or the images can't be composed, or an image is nonsensical (zero-size etc?)
     I can't imagine how a composition failure would happen in the current conception, but perhaps it's possible *)
  (* different assemblies will be needed depending on the relative size of the images *)
  (* the usual case will be that the center is largest, so we'll address that first *)
  (* whitespace = (center.width mod side.width); what to do if it's odd? right now,
     just let it be off center. *)
  let horiz_border_reps = border_repetitions ~center:center.width ~side:side.width in
  (* use the side's width measurement again, since we'll reflect it when blitting it *)
  let vert_border_reps = border_repetitions ~center:center.height ~side:side.width in
  let width = corner.width * 2 + horiz_border_reps * side.width in
  let height = corner.height * 2 + vert_border_reps * side.width in
  (* TODO:
     there is a difference between white stitch on white background, and no stitch.
     Gradations of alpha are not meaningful but full transparent vs full opaque is worth
     distinguishing. *)
  let canvas =
    Image.create_rgb ~max_val:(max3 center.max_val corner.max_val side.max_val)
      ~alpha:true width height
  in
  blit_corners corner canvas;
  blit_horiz_edges corner center side canvas;
  blit_vert_edges corner center side canvas;
  blit_center corner center side canvas;
  canvas
