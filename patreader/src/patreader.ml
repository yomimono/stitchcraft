let input =
  let doc = "file from which to read"
  and docv = "FILE" in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc ~docv)

let info =
  let doc = "read .pat files" in
  Cmdliner.Term.info "patreader" ~doc

let read input =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Input input >>= fun input ->
  Angstrom_lwt_unix.parse Patreader.file input >>= fun (_, result) ->
  match result with
  | Ok f ->
    Format.printf "%a\n%!" Patreader.pp_pattern f;
    Lwt.return (Ok ())
  | Error e -> Lwt.return @@ Error (`Msg e)

let main input =
  Lwt_main.run (read input)

let read_t = Cmdliner.Term.(const main $ input)

let () =
  Cmdliner.Term.eval (read_t, info) |> function
  | `Ok (Error (`Msg s)) ->
    Format.eprintf "%s\n%!" s; exit 1
  | `Ok (Ok ()) ->
    exit 0
  | `Error _ -> exit 1
  | e -> Cmdliner.Term.exit e
