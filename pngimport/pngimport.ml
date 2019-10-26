module BlockMap = Stitchy.Types.BlockMap
module ColorMap = Map.Make(Stitchy.RGB)

let file =
  let doc = "png to import" in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let prefix =
  let doc = "prefix for the output files. they will be named ${prefix}0.json , ${prefix}1.json, etc\
             with each file representing one color of stitches." in
  Cmdliner.Arg.(value & opt string "layer" & info ["p"; "prefix"] ~doc)

let background =
  let doc = "background color in 8-bit (r, g, b) format. Don't include a layer representing these pixels.  To disable background detection completely, specify a nonsense value like (999, 999, 999)." in
  Cmdliner.Arg.(value & opt (t3 int int int) (999, 999, 999) & info ["b"; "background"] ~doc)

let info =
  let doc = "make some stitchy-compatible pixel layers from a png image" in
  Cmdliner.Term.info "pngimport" ~doc

(* construct a map of colors to `RGB.t BlockMap.t`s; there's one set of blockmaps for each color. *)
(* each color will be independently serialized as a separate layer, to be assigned
   to a thread by a later assembling pass. *)
let count_colors image =
  (* (x, y) -> (r, g, b) *)
  let color_map = ColorMap.empty in
  let categorize_pixel x y colormap =
    Image.read_rgba image x y (fun r g b a ->
        (* ignore entirely transparent pixels *)
        match a with
        | 0 -> colormap
        | _ ->
          let blocklist =
            match ColorMap.find_opt (r, g, b) colormap with
            | None -> (x, y)::[]
            | Some l -> (x, y)::l
          in
          ColorMap.add (r, g, b) blocklist colormap
      )
  in
  let rec aux x y m =
    if x >= image.Image.width then aux 0 (y+1) m
    else if y >= image.Image.height then m
    else begin
      let m = categorize_pixel x y m in
      aux (x+1) y m
    end
  in
  aux 0 0 color_map

type layer = {
  color : Stitchy.RGB.t;
  stitches : (int * int) list;
  height : int;
  width : int;
} [@@deriving yojson]

let output layer name =
  let json = layer_to_yojson layer in
  Yojson.Safe.to_file name json

let go background file prefix =
  let reader =
    if 0 = String.compare file "-" then ImageUtil_unix.chunk_reader_of_in_channel stdin
    else ImageUtil_unix.chunk_reader_of_path file
  in
  let image = ImageLib.openfile ~extension:"png" reader in
  let color_map = count_colors image in
  let index = ref 0 in
  let write stitch_map n =
    output stitch_map (prefix ^ (string_of_int n))
  in
  ColorMap.iter (fun color stitches ->
      if Stitchy.RGB.compare color background != 0 then begin
        write {color; stitches; height = image.Image.height; width = image.Image.width} !index;
        index := !index + 1
      end)
    color_map

let go_t = Cmdliner.Term.(const go $ background $ file $ prefix )

let () = Cmdliner.(Term.exit @@ Term.eval (go_t, info))
