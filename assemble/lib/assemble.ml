open Stitchy.Types

let make_substrate background grid ~width ~height =
  { background;
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let stitch thread background gridsize blocks =
  let substrate = make_substrate background gridsize ~width:blocks.width ~height:blocks.height in
  let block : block = { thread; stitch = Full; } in
  let stitches = List.fold_left (fun m (x, y) -> 
      BlockMap.add (x, y) block m
    ) BlockMap.empty blocks.stitches
  in
  {stitches; substrate;}
