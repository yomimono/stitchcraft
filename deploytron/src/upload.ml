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

let stream_of_string x =
  let once = ref false in
  let go () =
    if !once
    then None
    else (
      once := true ;
      Some (x, 0, String.length x)) in
  go


let upload storage key shop listing filename =
  match Bos.OS.File.read Fpath.(v storage / "token") with
  | Error (`Msg s) -> Printf.eprintf "failed to find a token in %s : %s\n%!" storage s;
    exit 1
  | Ok token ->
    (* TODO parameterize rank; right now they're all 1 *)
    let rank = 1 in
    Lwt_main.run @@ (
      match Bos.OS.File.read (Fpath.v filename) with
      | Error (`Msg s) -> Printf.eprintf "couldn't read file for upload: %s\n%!" s; exit 1
      | Ok file ->
        Printf.printf "file size: %d\n%!" @@ String.length file;
        let headers = auth_headers key token in
        let uri = Etsy.File.upload_by_listing_id ~shop listing in
        (* in order to upload properly, we need multipart form data,
         * which cohttp doesn't directly provide.
         * dinosaure to the rescue ;) *)
        (* the (string * int * int ) in the part definition is buffer, offset, length *)
        let name_part, rank_part, file_part =
          let rank_str = string_of_int rank in
          let open Multipart_form in
          part ~encoding:`Bit7 @@ stream_of_string filename,
          part ~encoding:`Bit7 @@ stream_of_string rank_str,
          part ~encoding:`Binary ~disposition:(Content_disposition.v ~filename "file") @@
            stream_of_string file
        in
        Mirage_crypto_rng_unix.initialize ();
        let rng ?g:_ n = Mirage_crypto_rng_unix.getrandom n |> Cstruct.to_string |> Base64.encode_string ~alphabet:Base64.uri_safe_alphabet in
        Printf.printf "randomed\n%!";
        let multipart = Multipart_form.multipart ~rng [name_part; rank_part; file_part] in
        Printf.printf "multiparted\n%!";
        let h, form_body = Multipart_form.to_stream multipart in
        let content_type = Multipart_form.(Header.content_type h |> Content_type.to_string)
        and content_encoding = Multipart_form.(Header.content_encoding h |> Content_encoding.to_string)
        in
        let headers = Cohttp.Header.add_list headers
            ["Content-type", content_type;
             "Content-encoding", content_encoding;
            ] in
        let body = Lwt_stream.(from_direct form_body |> map (fun (buf, _, _) -> buf)) |> Cohttp_lwt.Body.of_stream in
        Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (response, body) ->
        Cohttp_lwt.Body.to_string body >>= fun contents ->
        success_or_death response contents >>= fun () ->
        Printf.printf "%s\n%!" contents;
        Lwt.return_unit
    )


let upload_t = Cmdliner.Term.(const upload $ storage $ key $ shop $ listing $ file)

let info = Cmdliner.Term.info "upload"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (upload_t, info)
