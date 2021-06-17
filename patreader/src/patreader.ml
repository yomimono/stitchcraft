let input =
  let doc = "file from which to read"
  and docv = "FILE" in
  Cmdliner.Arg.(value & pos_all string [] & info [] ~doc ~docv)

let info =
  let doc = "read .pat files" in
  Cmdliner.Term.info "patreader" ~doc

let match_thread palette_entry =
  let manufacturer = palette_entry.Patreader.scheme |> String.trim in
  if String.compare manufacturer "DMCDMC" = 0 then begin
    match Stitchy.DMC.Thread.of_string @@ String.trim palette_entry.Patreader.color_name with
    | Some thread -> Some thread
    | None ->
      let (r, g, b, _) = palette_entry.Patreader.color in
      Stitchy.DMC.Thread.of_rgb (r, g, b)
  end else
    None

let to_substrate fabric =
  let grid = match fabric.Patreader.cloth_count_width, fabric.Patreader.cloth_count_height with
    | 16, _ | _, 16 -> Stitchy.Types.Sixteen
    | 18, _ | _, 18 -> Stitchy.Types.Eighteen
    | _, _ -> Stitchy.Types.Fourteen
  in
  let (r, g, b, _) = fabric.Patreader.fabric_color in
  let max_x, max_y = fabric.Patreader.width - 1, fabric.Patreader.height - 1 in
  Stitchy.Types.{ grid; background = (r, g, b); max_x; max_y }

let to_stitches (_fabric, _metadata, palette, stitches) =
  (* We can map the palette entries to threads, but not to layers,
   * because each stitch carries its own stitch type (full cross-stitch,
   * half-stitch, etc), so one entry in the palette list may correspond
   * to multiple layers.
   *
   * We can still use the thread map to look up the stitch indices, 
   * so it's worth doing that mapping once for the pattern and then
   * referring to it later. *)
  let threads = List.map match_thread palette in
  List.fold_left (fun acc ((x, y), color_index, stitch) ->
      match acc, color_index with 
      | Error s, _ -> Error s
      (* color 0xff has a special meaning -- no stitch *)
      | Ok acc, 255 -> Ok acc
      | Ok acc, color_index ->
        match (List.nth threads (color_index - 1)) with
        | None -> Error (`Msg "unknown thread")
        | Some thread ->
          match List.partition (fun layer ->
              Stitchy.Types.(equal_stitch layer.stitch stitch) &&
              Stitchy.DMC.Thread.compare layer.thread thread = 0) acc with
          | layer::_, other_layers ->
            Ok ({ layer with stitches = Stitchy.Types.CoordinateSet.add (x, y) layer.stitches } :: other_layers)
          | [], other_layers ->
            let layer = Stitchy.Types.{
                thread; stitch; stitches = CoordinateSet.singleton (x, y)
              } in
            Ok (layer :: other_layers)
    ) (Ok []) stitches

let read_one input =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Input input >>= fun input ->
  Angstrom_lwt_unix.parse Patreader.file input >>= fun (_, result) ->
  match result with
  | Error e -> Lwt.return @@ Error (`Msg e)
  | Ok (fabric, metadata, palette, stitches, extras, knots, backstitches) ->
    Format.eprintf "metadata: %a\n%!" Patreader.pp_metadata metadata;
    Format.eprintf "fabric: %a\n%!" Patreader.pp_fabric fabric;
    Format.eprintf "palette: %a\n%!" Patreader.pp_palette palette;
    Format.eprintf "got %d extras\n%!" @@ List.length extras;
    Format.eprintf "got %d knots\n%!" @@ List.length knots;
    Format.eprintf "got %d backstitches\n%!" @@ List.length backstitches;
    let substrate = to_substrate fabric in
    let stitches = List.map (fun (coords, color, _stitch) ->
        coords, color, (Stitchy.Types.Cross Full)) stitches in
    match to_stitches (fabric, metadata, palette, stitches) with
    | Error e -> Lwt.return @@ Error e
    | Ok layers -> begin
      let pattern = {Stitchy.Types.substrate = substrate;
                     layers } in
      Format.printf "%s" (Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_string);
      Lwt.return (Ok ())
    end

let main inputs =
  List.fold_left (fun acc input ->
      let res = Lwt_main.run @@ read_one input in
      match acc, res with
      | e, Ok () -> e
      | Ok (), e -> e
      | Error (`Msg e1), Error (`Msg e2) -> Error (`Msg (e1 ^ "\n" ^ input ^ ": " ^ e2))
    ) (Ok ()) inputs

let read_t = Cmdliner.Term.(const main $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) ->
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
