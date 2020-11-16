let white = match Stitchy.DMC.Thread.of_rgb (255, 255, 255) with
  | None -> assert false
  | Some white -> white

let print_glyph fmt (glyph : Stitchy.Types.glyph) =
  let block : Stitchy.Types.block = {
    thread = white;
    stitch = Stitchy.Types.Full;
  } in
  let blockmap = List.fold_left
      (fun m coordinate -> Stitchy.Types.BlockMap.add coordinate block m)
      Stitchy.Types.BlockMap.empty
      glyph.stitches
  in
  let substrate : Stitchy.Types.substrate = {
    grid = Stitchy.Types.Fourteen;
    background = (0, 0, 0);
    max_x = glyph.width - 1;
    max_y = glyph.height - 1;
  } in
  let state : Stitchy.Types.state = { stitches = blockmap; substrate; } in
  Format.fprintf fmt "%a\n%!" Stitchy.Types.pp_state state

module type INTERPRETER = sig
    type glyphmap = Stitchy.Types.glyph list * Uchar.t list list
    type error
    val pp_error : Format.formatter -> error -> unit
    val glyphmap_of_buffer : Cstruct.t -> ([`Glyphmap of glyphmap], error) result
end

module Reader(Interpreter : INTERPRETER ) = struct
    let read input =
      match Bos.OS.File.read (Fpath.v input) with
      | Error e -> Error e
      | Ok s ->
        let buffer = Cstruct.of_string s in
        match Interpreter.glyphmap_of_buffer buffer with
        | Error e ->
          Format.eprintf "%a\n%!" Interpreter.pp_error e;
          Error (`Msg "parsing failed")
        | Ok (`Glyphmap (glyphs, unicode)) ->
          let spoo glyph uchars =
            let scratch = Buffer.create 16 in
            List.iter (fun uchar ->
                Buffer.reset scratch;
                Uutf.Buffer.add_utf_8 scratch uchar;
                Buffer.output_buffer stdout scratch;
                Stdlib.flush_all ();
              ) uchars;
            Format.printf
              " glyph: %a\n%!" print_glyph glyph;
          in
          List.iter2 spoo glyphs unicode;
          Ok ()
end
