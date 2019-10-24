open Stitchy.Types

let make_substrate grid phrase interline =
  let {Chars.height; width; _ } = Chars.get_dimensions phrase interline in
  { background = (255, 255, 255);
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let blocks_of_phrase block phrase interline =
  let add_blocks_for_letter ~x_off ~y_off letter blockmap =
    match Chars.CharMap.find_opt letter Chars.map with
    | None -> blockmap
    | Some pixels -> List.fold_left (fun m (x, y) ->
        BlockMap.add (x_off + x, y_off + y) block m
      ) blockmap pixels
  in
  let add_line ~y_off line blockmap =
    Astring.String.fold_left (fun (x_off, map) c ->
        let map = add_blocks_for_letter ~x_off ~y_off c map in
        (x_off + Chars.w, map)
      ) (0, blockmap) line
  in
  let blockmap = BlockMap.empty in
  List.fold_left (fun (y_off, map) line ->
      let next_y_off = y_off + (Chars.h + interline) in
      (next_y_off, (snd @@ add_line ~y_off line map))
    ) (0, blockmap) (Chars.get_dimensions phrase interline).lines

let stitch textcolor gridsize phrase interline =
  let thread = Colors.thread_of_color textcolor in
  let substrate = make_substrate gridsize phrase interline in
  let block : block = { thread; stitch = Full; } in
  let (_, phrase) = blocks_of_phrase block phrase interline in
  {stitches = phrase; substrate;}
