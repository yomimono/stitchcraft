let white = match Stitchy.DMC.Thread.of_rgb (255, 255, 255) with
  | None -> assert false
  | Some white -> white

let print_glyph fmt (glyph : Stitchy.Types.glyph) =
  let layer : Stitchy.Types.layer = {
    thread = white;
    stitch = Stitchy.Types.Cross Full;
    stitches = glyph.stitches;
  } in
  let substrate : Stitchy.Types.substrate = {
    grid = Stitchy.Types.Fourteen;
    background = (0, 0, 0);
    max_x = glyph.width - 1;
    max_y = glyph.height - 1;
  } in
  let pattern : Stitchy.Types.pattern = { layers = [layer]; substrate;
                                          backstitch_layers = [];} in
  Format.fprintf fmt "%a\n%!" Stitchy.Types.pp_pattern pattern

module type INTERPRETER = sig
    type glyphmap = (Stitchy.Types.glyph * Uchar.t list) list
    type error
    val pp_error : Format.formatter -> error -> unit
    (** [glyphmap_of_buffer debug buf] tries to extract a glyphmap from buf.
        Setting [debug] gives a lot of output on stdout. *)
    val glyphmap_of_buffer : bool -> Cstruct.t -> (glyphmap, error) result
end

module Reader(Interpreter : INTERPRETER ) = struct
  let read debug input =
    match Bos.OS.File.read (Fpath.v input) with
    | Error e -> Error e
    | Ok s ->
      let buffer = Cstruct.of_string s in
      match Interpreter.glyphmap_of_buffer debug buffer with
      | Error e ->
        Format.eprintf "%a\n%!" Interpreter.pp_error e;
        Error (`Msg "parsing failed")
      | Ok l ->
        if debug then Printf.printf "glyphmap read succeeded\n%!";
        let spoo (glyph, uchars) =
          let scratch = Buffer.create 16 in
          List.iter (fun uchar ->
              Buffer.reset scratch;
              Uutf.Buffer.add_utf_8 scratch uchar;
              Buffer.output_buffer stdout scratch;
              Stdlib.flush_all ();
              Format.printf " (int %d 0x%x) " (Uchar.to_int uchar) (Uchar.to_int uchar);
            ) uchars;
          Format.printf
            " glyph: %a\n%!" print_glyph glyph;
        in
        List.iter spoo l;
        Ok ()
end
