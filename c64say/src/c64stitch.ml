open Stitchy.Types
(* take a phrase, output a stitch file *)

let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])
let interline =
  let doc = "extra space to insert between lines (in stitches)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output = Cmdliner.Arg.(value & opt string "stitch.json" & info ["f"; "file"])

(* TODO: background and text color ought to be choosable from the known C64 colors. *)

(* TODO: grid size should be choosable from the known valid values. *)

let substrate_of_phrase phrase interline =
  let {C64chars.height; width; _ } = C64chars.get_dimensions phrase interline in
  { Stitchy.Types.background = (255, 255, 255);
    grid = Fourteen;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
    block_size = 1;
  }

let blocks_of_phrase block phrase interline =
  let add_blocks_for_letter ~x_off ~y_off letter blockmap =
    match C64chars.CharMap.find_opt letter C64chars.map with
    | None -> blockmap
    | Some pixels -> List.fold_left (fun m (x, y) ->
        BlockMap.add (x_off + x, y_off + y) block m
      ) blockmap pixels
  in
  let add_line ~y_off line blockmap =
    Astring.String.fold_left (fun (x_off, map) c ->
        let map = add_blocks_for_letter ~x_off ~y_off c map in
        (x_off + C64chars.w, map)
      ) (0, blockmap) line
  in
  let blockmap = Stitchy.Types.BlockMap.empty in
  List.fold_left (fun (y_off, map) line ->
      let next_y_off = y_off + (C64chars.h + interline) in
      (next_y_off, (snd @@ add_line ~y_off line map))
    ) (0, blockmap) (C64chars.get_dimensions phrase interline).lines

let stitch phrase interline output =
  match Stitchy.DMC.Thread.of_rgb (0, 0, 0) with
  | None -> failwith "oh no"
  | Some thread ->
    let substrate = substrate_of_phrase phrase interline in
    let block : Stitchy.Types.block = { thread;
                                        stitch = Full; } in
    let (_, phrase) = blocks_of_phrase block phrase interline in
    let state = {stitches = phrase; substrate;} in
    Yojson.Safe.to_file output (Stitchy.Types.state_to_yojson state)

let stitch_t = Cmdliner.Term.(const stitch $ phrase $ interline $ output)

let info =
  let doc = "make a stitch file repesenting a phrase in c64 font" in
  Cmdliner.Term.info "c64stitch" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (stitch_t, info)
