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

let eq_layers_disregarding_stitch_set layer stitch thread =
  Stitchy.Types.(equal_stitch layer.stitch stitch) &&
  Stitchy.DMC.Thread.compare layer.thread thread = 0

let add_stitches layers thread stitch new_stitches =
  match List.partition (fun layer ->
      eq_layers_disregarding_stitch_set layer stitch thread) layers with
  | layer::_, other_layers ->
    let stitches =
      Stitchy.Types.CoordinateSet.(union layer.stitches @@ of_list new_stitches) in
    Ok ({ layer with stitches } :: other_layers)
  | [], other_layers ->
    let layer = Stitchy.Types.{
        thread; stitch; stitches = CoordinateSet.of_list new_stitches
      } in
    Ok (layer :: other_layers)

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

(* the position numbers *appear* to be:
 1  2  3
 4  5  6
 7  8  9 *)

let backstitch_of_position_pair backstitch =
  let open Patreader in
  match backstitch.start_position, backstitch.end_position with
  | (1, 3) | (3, 1) when backstitch.start_y = backstitch.end_y -> Ok Stitchy.Types.Top
  | (1, 1) | (3, 3) when backstitch.start_y = backstitch.end_y -> Ok Stitchy.Types.Top
  | (7, 9) | (9, 7) when backstitch.start_y = backstitch.end_y -> Ok Stitchy.Types.Bottom
  | (7, 7) | (9, 9) when backstitch.start_y = backstitch.end_y -> Ok Stitchy.Types.Bottom
  | (1, 7) | (7, 1) when backstitch.start_x = backstitch.end_x -> Ok Stitchy.Types.Left
  | (1, 1) | (3, 3) when backstitch.start_x = backstitch.end_x -> Ok Stitchy.Types.Left
  | (3, 9) | (9, 3) when backstitch.start_x = backstitch.end_x -> Ok Stitchy.Types.Right
  | (7, 7) | (9, 9) when backstitch.start_x = backstitch.end_x -> Ok Stitchy.Types.Right
  | _ -> Error (`Msg (Format.asprintf "unrepresentable backstitch %a" pp_backstitch backstitch))

let stitches_of_backstitch backstitch =
  let open Rresult.R in
  let open Patreader in
  let extra_stitch = match backstitch.start_position, backstitch.end_position with
    | (1, 3) | (3, 1) -> 1
    | (1, 7) | (7, 1) -> 1
    | (3, 9) | (9, 3) -> 1
    | (7, 9) | (9, 7) -> 1
    | _ -> 0
  in
  let diff one two = (abs (two - one)) + extra_stitch in
  let horizontal backstitch =
    let range = diff backstitch.end_x backstitch.start_x in
    List.init range (fun more_x -> (backstitch.start_x - 1 + more_x, backstitch.start_y - 1))
  and vertical backstitch =
    let range = diff backstitch.end_y backstitch.start_y in
    List.init range (fun more_y -> (backstitch.start_x - 1, backstitch.start_y - 1 + more_y)) 
  in
  backstitch_of_position_pair backstitch >>| function
  | Top -> Stitchy.Types.Back Top, horizontal backstitch
  | Bottom -> Stitchy.Types.Back Bottom, (horizontal backstitch)
  | Left -> Stitchy.Types.Back Left, (vertical backstitch)
  | Right -> Stitchy.Types.Back Right, (vertical backstitch)

let layers_of_backstitches threads backstitches =
  let open Rresult.R in
  let one_backstitch acc backstitch =
    match acc with
    | Error e -> Error e
    | Ok layers ->
      match (List.nth threads (backstitch.Patreader.color_index - 1)) with
      | None -> Error (`Msg "unknown thread")
      | Some thread ->
        match stitches_of_backstitch backstitch with
        | Ok (stitch, stitches) ->
          Format.eprintf "interpreting backstitch %a as stitch %a for coords\n%a\n%!"
            Patreader.pp_backstitch backstitch
            Stitchy.Types.pp_stitch stitch
            Fmt.(list @@ parens @@ pair ~sep:Fmt.comma int int) stitches;
          add_stitches layers thread stitch stitches >>= fun layers ->
          Ok layers
        | Error (`Msg s) ->
          Format.eprintf "%s\n%!" s;
          acc
  in
  List.fold_left one_backstitch (Ok []) backstitches

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
  layers_of_backstitches threads backstitches >>= fun backstitch_layers ->
  Ok (stitch_layers @ backstitch_layers)

