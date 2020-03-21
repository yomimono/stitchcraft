let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)  

let info =
  let doc = "ingest psf (pc screen font) version 2 files" in
  Cmdliner.Term.info "readpsf" ~doc

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

let read input =
  match Bos.OS.File.read (Fpath.v input) with
  | Error e -> Error e
  | Ok s ->
    let buffer = Cstruct.of_string s in
    match Psf2stitchfont.glyphmap_of_psf_header buffer with
    | Error e ->
      Format.eprintf "%a\n%!" Psf2stitchfont.pp_error e;
      Error (`Msg "parsing failed")
    | Ok (`Glyphmap (glyphs, unicode)) ->
      let spoo glyph uchars =
        List.iter (fun uchar ->
            match Uutf.(encode (encoder `UTF_8 (`Channel stdout)) (`Uchar uchar)) with
            | `Ok -> Format.printf "%a\n%!" Fmt.Dump.uchar uchar; ()
            | `Partial -> assert false
          ) uchars;
        Pervasives.flush_all ();
        Format.printf "\nglyph: %a\n%!" print_glyph glyph
      in
      List.iter2 spoo glyphs unicode;
      Ok ()

let read_t = Cmdliner.Term.(const read $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) -> 
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0 
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
