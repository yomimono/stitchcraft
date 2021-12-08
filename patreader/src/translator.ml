let add_stitches layers thread stitch_type new_stitches =
  let eq_layers_disregarding_stitch_set layer stitch_type thread =
    Stitchy.Types.(equal_stitch layer.stitch stitch_type) &&
    Stitchy.DMC.Thread.compare layer.thread thread = 0
  in
  match List.partition (fun layer ->
      eq_layers_disregarding_stitch_set layer stitch_type thread) layers with
  | layer::_, other_layers ->
    let stitches =
      Stitchy.Types.CoordinateSet.(union layer.stitches @@ of_list new_stitches) in
    Ok ({ layer with stitches } :: other_layers)
  | [], other_layers ->
    let layer = Stitchy.Types.{
        thread; stitch = stitch_type; stitches = CoordinateSet.of_list new_stitches
      } in
    Ok (layer :: other_layers)

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

let layers_of_cross_stitches threads stitches =
  List.fold_left (fun acc ((x, y), color_index, stitch) ->
      match acc, color_index with 
      | Error s, _ -> Error s
      (* color 0xff has a special meaning -- no stitch *)
      (* color indices are *supposed* to be 1-indexed, but I keep running into
       * stuff that's like "hey here is color index 0", what the crap.
       * so let's also treat that as "no stitch" *)
      | Ok acc, 255 | Ok acc, 0 -> Ok acc
      | Ok acc, color_index ->
        match (List.nth threads (color_index - 1)) with
        | None -> Error (`Msg "unknown thread")
        | Some thread ->
          add_stitches acc thread stitch [(x, y)]
    ) (Ok []) stitches

let stitchyfy backstitch =
  let open Patreader in
  (* given a position on the pixel grid (which consists of the gaps formed by the grid)
   * and a "position" 1-9, assign a position on the backstitch grid. *)
  (* nb that "x" and "y" here are from a 1-indexed pixel grid,
   * which we're translating to a 0-indexed vertex grid.
   * If both grids had the same index, position 1 would require
   * no adjustment. *)
  let corner_position (x, y) = function
    | 1 -> Some (x - 1, y - 1)
    | 3 -> Some (x, y - 1)
    | 7 -> Some (x - 1, y)
    | 9 -> Some (x, y)
    | _ -> None
  in
  let src = corner_position (backstitch.start_x, backstitch.start_y) backstitch.start_position
  and dst = corner_position (backstitch.end_x, backstitch.end_y) backstitch.end_position
  in
  match src, dst with
  | Some src, Some dst -> Some (backstitch.color_index, src, dst)
  | _, _ -> None

let backstitch_layers_of_backstitches threads backstitches =
  let translated_backstitches = List.filter_map stitchyfy backstitches in
  let layerfy (color_index, src, dst) =
    match (List.nth threads (color_index - 1)) with
    | None -> None
    | Some thread ->
      Some {Stitchy.Types.thread; stitches = Stitchy.Types.SegmentSet.singleton (src, dst)}
  in
  Ok (List.filter_map layerfy translated_backstitches)

let to_stitches (_fabric, _metadata, palette, stitches, backstitches) =
  let open Rresult.R in
  (* We can map the palette entries to threads, but not to layers,
   * because each stitch carries its own stitch type (full cross-stitch,
   * half-stitch, etc), so one entry in the palette list may correspond
   * to multiple layers.
   *
   * We can still use the thread map to look up the stitch indices, 
   * so it's worth doing that mapping once for the pattern and then
   * referring to it later. *)
  let threads = List.map match_thread palette in
  layers_of_cross_stitches threads stitches >>= fun stitch_layers ->
  backstitch_layers_of_backstitches threads backstitches >>= fun backstitch_layers ->
  Ok (stitch_layers, backstitch_layers)

