let verbose =
  let doc = "print file metadata on stderr"
  and docv = "VERBOSE" in
  Cmdliner.Arg.(value & flag & info ["v";"verbose"] ~doc ~docv)

let info =
  let doc = "read .pat files" in
  Cmdliner.Cmd.info "patreader" ~doc

let spoo (fabric, metadata, palette, stitches, extras, knots, backstitches) =
  Format.eprintf "metadata: %a\n%!" Patreader.Patfile.pp_metadata metadata;
  Format.eprintf "fabric: %a\n%!" Patreader.Patfile.pp_fabric fabric;
  Format.eprintf "palette: %a\n%!" Patreader.Patfile.pp_palette palette;
  Format.eprintf "got %d stitches\n%!" @@ List.length stitches;
  Format.eprintf "got %d extras\n%!" @@ List.length extras;
  Format.eprintf "got %d knots\n%!" @@ List.length knots;
  Format.eprintf "got %d backstitches\n%!" @@ List.length backstitches;
  ()

let read_one verbose filename =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Input filename >>= fun input ->
  if verbose then Format.eprintf "beginning parse for file %s\n%!" filename;
  Angstrom_lwt_unix.parse Patreader.Patfile.file input >>= fun (_, result) ->
  match result with
  | Error _ as e -> Lwt.return e
  | Ok (fabric, metadata, palette, stitches, extras, knots, backstitches) ->
    if verbose then spoo (fabric, metadata, palette, stitches, extras, knots, backstitches);
    let substrate = Patreader.Translate.to_substrate fabric in
    (* TODO: this is probably not strictly correct; I think some entries in the
     * stitch list can be half, 3/4, etc stitches *)
    let stitches = List.map (fun (coords, color, _stitch) ->
        coords, color, (Stitchy.Types.Cross Full)) stitches in
    match Patreader.Translate.to_stitches (fabric, metadata, palette, stitches, backstitches) with
    | Error (`Msg e) -> Lwt.return @@ Error e
    | Ok (layers, backstitch_layers) -> begin
      let pattern = {Stitchy.Types.substrate = substrate;
                     backstitch_layers;
                     layers } in
      Yojson.Safe.to_channel stdout (Stitchy.Types.pattern_to_yojson pattern);
      Lwt.return (Ok ())
    end

let main verbose input =
  let res = Lwt_main.run @@ read_one verbose input in
  match res with
    | Error e -> Format.eprintf "%s\n%!" e; exit 1
    | Ok _ when verbose -> Format.printf "successfully completed\n%!"
    | Ok _ -> ()
