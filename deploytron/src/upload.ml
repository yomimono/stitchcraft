open Lwt.Infix

let storage =
  let doc = "directory for storing tokens" in
  let default = match Bos.OS.Dir.user () with
    | Ok homedir -> Fpath.(homedir / ".config" / "stitchcraft")
    | Error _ -> Fpath.v "."
  in
  Cmdliner.Arg.(value & opt dir (Fpath.to_string default) & info ~doc ["d"; "dir"])

let key =
  let doc = "x-api-key for OpenAPI requests" in
  Cmdliner.Arg.(value & pos 0 string "not-a-real-api-key" & info ~doc ~docv:"KEY" [])

let shop =
  let doc = "shop ID to manage" in
  Cmdliner.Arg.(value & pos 1 int 1234 & info ~doc ~docv:"SHOP" [])

let listing =
  let doc = "listing ID to manage" in
  Cmdliner.Arg.(value & pos 2 int 3456 & info ~doc ~docv:"LISTING" [])

let file =
  let doc = "file to upload" in
  Cmdliner.Arg.(value & pos 3 file "example.pdf" & info ~doc ~docv:"FILE" [])

let auth_headers key token =
  let header = Cohttp.Header.init_with "x-api-key" key in
  Cohttp.Header.add header "Authorization" ("Bearer " ^ token)

let success_or_death response contents =
  match Cohttp.Response.status response |> Cohttp.Code.code_of_status with
  | 200 | 201 -> Lwt.return_unit
  | 400 -> Printf.eprintf "problem with the request data: %s\n%!" contents;
    exit 1
  | 404 -> Printf.eprintf "not found: %s\n%!" contents;
    exit 1
  | 500 -> Printf.eprintf "internal server error: %s\n%!" contents;
    exit 1
  | n -> Printf.eprintf "unexpected code %d: %s\n%!" n contents;
    exit 1

let upload storage key shop listing filename =
  match Bos.OS.File.read Fpath.(v storage / "token") with
  | Error (`Msg s) -> Printf.eprintf "failed to find a token in %s : %s\n%!" storage s;
    exit 1
  | Ok token ->
    (* TODO parameterize listing file id; right now they're all 1 *)
    Lwt_main.run @@ (
      match Bos.OS.File.read (Fpath.v filename) with
      | Error (`Msg s) -> Printf.eprintf "couldn't read file for upload: %s\n%!" s; exit 1
      | Ok file ->
        let headers = auth_headers key token in
        let uri = Etsy.File.upload_by_listing_id ~shop listing in
        (* in order to upload properly, we need multipart form data,
         * which cohttp doesn't directly provide. 
         * dinosaure to the rescue ;) *)
        let params = ["name", [filename];
                      "rank", ["1"];
                      "file", [file];
                     ] in
        Cohttp_lwt_unix.Client.post_form ~headers ~params uri >>= fun (response, body) ->
        Cohttp_lwt.Body.to_string body >>= fun contents ->
        success_or_death response contents >>= fun () ->
        Printf.printf "%s\n%!" contents;
        Lwt.return_unit
    )


let upload_t = Cmdliner.Term.(const upload $ storage $ key $ shop $ listing $ file)

let info = Cmdliner.Term.info "upload"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (upload_t, info)
