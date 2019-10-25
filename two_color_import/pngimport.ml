module ColorMap = Map.Make(Stitchy.RGB)

let file =
  let doc = "png to import" in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let background =
  let doc = "an (r, g, b) tuple representing the background color." in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b"; "background"] ~doc)

let info =
  let doc = "make some stitchy-compatible pixel layers from a png image" in
  Cmdliner.Term.info "pngimport" ~doc

(* construct a map of colors to `RGB.t BlockMap.t`s; there's one set of blockmaps for each color. *)
(* each color will be independently serialized as a separate layer, to be assigned
   to a thread by a later assembling pass. *)
let count_colors image =
  (* TODO: is there a quicker way to get the palette than running through the pixmap? *)
  let color_map = ColorMap.empty in
  let categorize_pixel x y m =
    Image.read_rgba image x y (fun r g b a ->
        (* ignore entirely transparent pixels *)
        match a with
        | 0 -> m
        | _ ->
          let blockmap = match ColorMap.find_opt (r, g, b) m with
            | Some blockmap -> blockmap
            | None -> 0
          in
          ColorMap.add (r, g, b) n m
      )
  in
  let rec aux x y m =
    if x >= image.Image.width then aux 0 (y+1) m
    else if y >= image.Image.height then m
    else begin
      let m = count_color x y m in
      aux (x+1) y m
    end
  in
  aux 0 0 color_count

let go file background =
  let reader =
    if 0 = String.compare file "-" then ImageUtil_unix.chunk_reader_of_in_channel stdin
    else ImageUtil_unix.chunk_reader_of_path file
  in
  let image = ImageLib.openfile ~extension:"png" reader in
  let substrate : Stitchy.Types.substrate = {
    background;
    grid = Stitchy.Types.Fourteen;
    max_x = max 0 (image.Image.width - 1);
    max_y = max 0 (image.Image.height- 1);
  } in
  let color_count = count_colors image in

let go_t = Cmdliner.Term.(const go $ file $ background)

let () = Cmdliner.(Term.exit @@ Term.eval (go_t, info))
