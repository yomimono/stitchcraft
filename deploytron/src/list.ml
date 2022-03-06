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
  Cmdliner.Arg.(value & pos 0 string "not-a-real-api-key" & info ~doc [])

let shop =
  let doc = "shop ID to manage" in
  Cmdliner.Arg.(value & pos 1 string "my-shop-id" & info ~doc [])

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

let get_shop token key user_id =
  let headers = auth_headers key token in
  let uri = Etsy.Shop.get_by_user_id user_id in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (response, body) ->
  Cohttp_lwt.Body.to_string body >>= fun contents ->
  success_or_death response contents >>= fun () ->
  match Etsy.Shop.of_yojson (Yojson.Safe.from_string contents) with
  | Error _ -> Printf.eprintf "error parsing json returned. raw json: %s" contents;
    exit 1
  | Ok shop -> Lwt.return shop

let get_listings token key shop_id =
  let headers = auth_headers key token in
  let uri = Etsy.Listing.get_listings_by_shop shop_id in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (response, body) ->
  Cohttp_lwt.Body.to_string body >>= fun contents ->
  success_or_death response contents >>= fun () ->
  match Etsy.Listing.many_of_yojson (Yojson.Safe.from_string contents) with
  | Error _ -> Printf.eprintf "error parsing json returned. raw json: %s" contents;
    exit 1
  | Ok listings -> Lwt.return listings

let get_files token key ~shop listing =
  let headers = auth_headers key token in
  let uri = Etsy.File.get_files_by_listing ~shop listing in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (response, body) ->
  Cohttp_lwt.Body.to_string body >>= fun contents ->
  success_or_death response contents >>= fun () ->
  match Etsy.File.many_of_yojson (Yojson.Safe.from_string contents) with
  | Error _ -> Printf.eprintf "error parsing json returned. raw json: %s" contents;
    exit 1
  | Ok files -> Lwt.return files
    
let ls storage key _shop =
  let open Lwt.Infix in
  match Bos.OS.File.read Fpath.(v storage / "token") with
  | Error (`Msg s) -> Printf.eprintf "failed to find a token in %s : %s\n%!" storage s;
    Error s
  | Ok token ->
    (* user ID is the first chunk of a token *)
    match String.split_on_char '.' token with
    | [] -> Printf.eprintf "token seems to lack a user ID\n%!"; exit 1
    | l when Stdlib.List.length l > 2 ->
      Printf.eprintf "token seems to have extraneous .'s, which are field separators\n%!"; exit 1
    | user_id::_ ->
      let _l = Lwt_main.run @@ (
        get_shop token key user_id >>= fun shop ->
        Format.printf "%a\n%!" Yojson.Safe.pp (Etsy.Shop.to_yojson shop);
        get_listings token key shop.shop_id >>= fun listings ->
        Lwt_list.map_s (fun (listing : Etsy.Listing.t) ->
            let id = listing.listing_id in
            get_files token key ~shop:shop.shop_id id >>= fun files ->
            Format.printf "%a\n%!" Yojson.Safe.pp (Etsy.File.many_to_yojson files);
            Lwt.return_unit
          ) listings.results
      ) in
      Ok ()

let ls_t = Cmdliner.Term.(const ls $ storage $ key $ shop)

let info = Cmdliner.Cmd.info "ls"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info ls_t
