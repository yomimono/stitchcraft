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
      | Ok acc, 255 -> Ok acc
      | Ok acc, color_index ->
        match (List.nth threads (color_index - 1)) with
        | None -> Error (`Msg "unknown thread")
        | Some thread ->
          add_stitches acc thread stitch [(x, y)]
    ) (Ok []) stitches

let to_stitches (_fabric, _metadata, palette, stitches, _backstitches) =
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
  Ok stitch_layers

