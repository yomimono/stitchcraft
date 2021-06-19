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

let backstitch_of_position_pair backstitch =
  let open Patreader in
  match backstitch.start_position, backstitch.end_position with
  | (1, 3) | (3, 1) -> Ok Stitchy.Types.Top
  | (1, 7) | (7, 1) -> Ok Stitchy.Types.Left
  | (3, 9) | (9, 3) -> Ok Stitchy.Types.Right
  | (7, 9) | (9, 7) -> Ok Stitchy.Types.Bottom
  | (1, 1) -> if backstitch.start_y == backstitch.end_y then Ok Stitchy.Types.Top else Ok Stitchy.Types.Left
  | (7, 7) -> if backstitch.start_y == backstitch.end_y then Ok Stitchy.Types.Bottom else Ok Stitchy.Types.Left
  | (3, 3) -> if backstitch.start_y == backstitch.end_y then Ok Stitchy.Types.Top else Ok Stitchy.Types.Right
  | (9, 9) -> if backstitch.start_y == backstitch.end_y then Ok Stitchy.Types.Bottom else Ok Stitchy.Types.Right
  | _ -> Ok Stitchy.Types.Top (* TODO: this is obviously wrong *)
  (* | (x, y) -> Error (`Msg (Format.asprintf "unrepresentable backstitch %d, %d" x y)) *)

let stitches_of_backstitch backstitch =
  let open Rresult.R in
  let open Patreader in
  let diff one two = (max one two) + 1 - (min one two) in
  let horizontal backstitch =
    let range = diff backstitch.end_x backstitch.start_x in
    List.init range (fun more_x -> (backstitch.start_x + more_x, backstitch.start_y)) 
  and vertical backstitch =
    let range = diff backstitch.end_y backstitch.start_y in
    List.init range (fun more_y -> (backstitch.start_x , backstitch.start_y + more_y)) 
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
        stitches_of_backstitch backstitch >>= fun (stitch, stitches) ->
        add_stitches layers thread stitch stitches >>= fun layers ->
        Ok layers
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
    (* TODO: this is probably not strictly correct; I think some entries in the
     * stitch list can be half, 3/4, etc stitches *)
    let stitches = List.map (fun (coords, color, _stitch) ->
        coords, color, (Stitchy.Types.Cross Full)) stitches in
    match to_stitches (fabric, metadata, palette, stitches, backstitches) with
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
