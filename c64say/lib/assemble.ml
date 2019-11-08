open Stitchy.Types

let make_substrate background grid phrase interline =
  let {Chars.height; width; _ } = Chars.get_dimensions phrase interline in
  { background;
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let blocks_of_phrase block phrase interline =
  let add_blocks_for_letter ~x_off ~y_off letter blockmap =
    match Chars.CharMap.find_opt letter Chars.map with
    | None -> Printf.eprintf "couldn't draw letter %x\n%!" (Uchar.to_int letter); blockmap
    | Some layer -> List.fold_left (fun m (x, y) ->
        BlockMap.add (x_off + x, y_off + y) block m
      ) blockmap layer.Stitchy.Types.stitches
  in
  let add_line ~y_off line blockmap =
    let decoder = Uutf.decoder (`String line) in
    let rec advance x_off map =
      match Uutf.decode decoder with
      | `End -> map
      | `Uchar uchar ->
          let map = add_blocks_for_letter ~x_off ~y_off uchar map in
          advance (x_off + Chars.w) map
      | `Malformed _ | `Await -> (* Await is nonsensical since we already have the whole string
                                  [decode] guarantees that "repeated invocation always eventually
                                  returns [`End], even in case of errors", so let's just do that *)
        advance x_off map
    in
    advance 0 blockmap
  in
  let blockmap = BlockMap.empty in
  List.fold_left (fun (y_off, map) line ->
      let next_y_off = y_off + (Chars.h + interline) in
      (next_y_off, add_line ~y_off line map)
    ) (0, blockmap) (Chars.get_dimensions phrase interline).lines

let stitch textcolor background gridsize phrase interline =
  let thread = Colors.thread_of_color textcolor in
  let substrate = make_substrate background gridsize phrase interline in
  let block : block = { thread; stitch = Full; } in
  let (_, phrase) = blocks_of_phrase block phrase interline in
  {stitches = phrase; substrate;}
