let uri_conv : Uri.t Cmdliner.Arg.conv =
  let parse s =
    try `Ok (Uri.of_string s) with
    | Invalid_argument s -> `Error s
  in
  let print = Uri.pp in
  (parse, print)

let default_uri = Uri.of_string "https://stitchbridge.legitcreds.us"

let host =
  let doc = "stitchcraft OAuth2 resource server" in
  Cmdliner.Arg.(value & opt uri_conv default_uri & info ~doc ["url";"uri"])

let storage =
  let doc = "directory for storing tokens" in
  let default = match Bos.OS.Dir.user () with
    | Ok homedir -> Fpath.(homedir / ".config" / "stitchcraft")
    | Error _ -> Fpath.v "."
  in
  Cmdliner.Arg.(value & opt dir (Fpath.to_string default) & info ~doc ["d"; "dir"])

let start_over =
  let doc = "start the whole thing over" in
  Cmdliner.Arg.(value & flag & info ~doc ["n"; "new"])

let state dir =
  match Bos.OS.Dir.create dir with
  | Error _ -> `Broken
  | Ok true -> `Nothing
  | Ok false ->
    match Bos.OS.File.read Fpath.(dir / "token") with
    | Ok token -> `Have_token token
    | Error _ ->
      match Bos.OS.File.read Fpath.(dir / "state") with
      | Ok state -> `Have_state state
      | Error _ -> `Nothing

let get host storage start_over =
  let open Lwt.Infix in
  let state_uri = Uri.with_path host "/auth" in
  let token_uri = Uri.with_path host "/token" in
  Lwt_main.run @@ (
    match state (Fpath.v storage), start_over with
    | `Broken, _ -> Printf.eprintf "Can't find or write %s; won't continue" storage; exit 1
    | `Have_token token, false -> Printf.printf "%s\n%!" token; exit 0
    | `Have_state state, false -> begin
        let params = ["state", [state]] in
        Cohttp_lwt_unix.Client.post_form ~params token_uri >>= fun (response, body) ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        match Cohttp.Response.status response with
        | `OK -> begin
            match Bos.OS.File.write Fpath.(v storage / "token") body with
            | Error (`Msg s) -> Printf.eprintf "error writing token: %s\n%!" s; exit 1
            | Ok () -> exit 0
          end
        | _ -> Printf.eprintf "remote server didn't give a token: %s\n%!" body;
          exit 1
      end
    | `Nothing, _ | _, true ->
      let params = ["uuid", ["sixteenbytesoffun"]] in
      Cohttp_lwt_unix.Client.post_form ~params state_uri >>= fun (response, body) ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let headers = Cohttp.Response.headers response in
      let () =
        match Cohttp.Header.get headers "Location" with
        | Some redirect -> begin
            match Bos.OS.File.write Fpath.(v storage / "state") body with
            | Error (`Msg s) -> Printf.eprintf "got a redirect from the remote server, but couldn't save the state it sent us: %s" s;
            | Ok () ->
              Printf.printf "%s\n%!" redirect;
          end
        | None -> match Cohttp.Response.status response with
          | `OK -> Printf.printf "%s\n%!" body;
          | _ -> Printf.eprintf "error: %s\n%!" body;
      in
      Lwt.return_unit
  )

let get_t = Cmdliner.Term.(const get $ host $ storage $ start_over)

let info = Cmdliner.Term.info "get"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (get_t, info)
