let input =
  let doc = "file from which to read"
  and docv = "FILE" in
  Cmdliner.Arg.(value & pos_all string [] & info [] ~doc ~docv)

let info =
  let doc = "read .pat files" in
  Cmdliner.Term.info "patreader" ~doc

let read_one input =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Input input >>= fun input ->
  Angstrom_lwt_unix.parse Patreader.file input >>= fun (_, result) ->
  match result with
  | Ok (fabric, metadata, palette, stitches) ->
    Format.printf "metadata: %a\n%!" Patreader.pp_metadata metadata;
    Format.printf "fabric: %a\n%!" Patreader.pp_fabric fabric;
    Format.printf "palette: %a\n%!" Patreader.pp_palette palette;
    Format.printf "got %d stitches\n%!" @@ List.length stitches;
    Lwt.return (Ok ())
  | Error e -> Lwt.return @@ Error (`Msg e)

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
