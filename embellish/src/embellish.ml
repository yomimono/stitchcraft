module LoadPNG = struct
  let open_png file =
    ImagePNG.ReadPNG.parsefile @@
    ImageUtil_unix.chunk_reader_of_path file

  let start_composition corner side center output =
    let out = Compose.compose (open_png corner) (open_png side) (open_png center) in
    ImagePNG.write_png output out

end

open Cmdliner

let corner =
  let doc = "Corner border image (oriented to upper-left corner).  Will be flipped (not rotated) as appropriate for other corners." in
  Arg.(value & opt file "corner.png" & info ["corner"] ~docv:"CORNER" ~doc)

let side =
  let doc = "Side border image (oriented horizontally).  Will be flipped (not rotated) as appropriate for other sides." in
  Arg.(value & opt file "side.png" & info ["side"] ~docv:"SIDE" ~doc)

let center =
  let doc = "Center image.  Corner and side will be inserted to surround this image." in
  Arg.(value & opt file "center.png" & info ["center"] ~docv:"CENTER" ~doc)

let output =
  let doc = "Path at which to output the finished, embellished image." in
  Arg.(value & opt string "embellished.png" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let go corner side center output =
  LoadPNG.start_composition corner side center (ImageUtil_unix.chunk_writer_of_path output)

let compose_t = Term.(const go $ corner $ side $ center $ output)

let info =
  let doc = "embellish an image with corner and border images" in
  Term.info "embellish" ~doc ~exits:Term.default_exits 

let () = Term.exit @@ Term.eval (compose_t, info)
