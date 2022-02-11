let storage =
  let doc = "directory for storing tokens" in
  let default = match Bos.OS.Dir.user () with
    | Ok homedir -> Fpath.(homedir / ".config" / "stitchcraft")
    | Error _ -> Fpath.v "."
  in
  Cmdliner.Arg.(value & opt dir (Fpath.to_string default) & info ~doc ["d"; "dir"])

let key =
  let doc = "x-api-key for OpenAPI requests" in
  Cmdliner.Arg.(value & pos 0 string "not-a-real-api-key" & info ~doc [])

let shop =
  let doc = "shop ID to manage" in
  Cmdliner.Arg.(value & pos 1 string "my-shop-id" & info ~doc [])

let auth_headers key token =
  let header = Cohttp.Header.init_with "x-api-key" key in
  Cohttp.Header.add header "Authorization" ("Bearer " ^ token)
    
let ls storage key _shop =
  let open Lwt.Infix in
  match Bos.OS.File.read Fpath.(v storage / "token") with
  | Error (`Msg s) -> Printf.eprintf "failed to find a token in %s : %s\n%!" storage s;
    exit 1
  | Ok token ->
    (* user ID is the first chunk of a token *)
    match String.split_on_char '.' token with
    | [] -> Printf.eprintf "token seems to lack a user ID\n%!"; exit 1
    | l when List.length l > 2 ->
      Printf.eprintf "token seems to have extraneous .'s, which are field separators\n%!"; exit 1
    | user_id::_ ->
      Lwt_main.run @@ (
        let uri = Etsy.User.get_by_id user_id in
        let headers = auth_headers key token in
        Cohttp_lwt_unix.Client.get ~headers uri >>= fun (response, body) ->
        Cohttp_lwt.Body.to_string body >>= fun contents ->
        match Cohttp.Response.status response |> Cohttp.Code.code_of_status with
        | 200 | 201 -> Printf.printf "%s\n%!" contents; Lwt.return_unit
        | 400 -> Printf.eprintf "problem with the request data: %s\n%!" contents;
          exit 1
        | 404 -> Printf.eprintf "not found: %s\n%!" contents;
          exit 1
        | 500 -> Printf.eprintf "internal server error: %s\n%!" contents;
          exit 1
        | n -> Printf.eprintf "unexpected code %d: %s\n%!" n contents;
          exit 1
      )

let ls_t = Cmdliner.Term.(const ls $ storage $ key $ shop)

let info = Cmdliner.Term.info "ls"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (ls_t, info)
