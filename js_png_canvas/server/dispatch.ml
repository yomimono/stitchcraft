open Lwt.Infix

(** Common signature for http and https. *)
module type HTTP = Cohttp_lwt.S.Server

(* Logging *)
let https_src = Logs.Src.create "https" ~doc:"HTTPS server"
module Https_log = (val Logs.src_log https_src : Logs.LOG)

let http_src = Logs.Src.create "database" ~doc:"Database operations"
module DB_log = (val Logs.src_log http_src : Logs.LOG)

module Dispatch
    (KV: Mirage_kv_lwt.RO) (* this key-value store is for serving web pages,
                              not storing or fetching data *)
    (S: HTTP) = struct

  let failf fmt = Fmt.kstrf Lwt.fail_with fmt

  let read_or_fail t name =
    let name = Mirage_kv.Key.v name in
    KV.get t name >>= function
    | Error e -> failf "read: %a" KV.pp_error e
    | Ok buf -> Lwt.return buf

  (* given a URI, find the appropriate file,
   * and construct a response with its contents. *)
  let rec dispatcher kv uri =
    match Uri.path uri with
    | "" | "/" -> dispatcher kv (Uri.with_path uri "index.html")
    | "/index.html" | "/grid.js" as path ->
      let header =
        Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000"
      in
      let mimetype = Magic_mime.lookup path in
      let headers = Cohttp.Header.add header "content-type" mimetype in
      Lwt.catch
        (fun () ->
           read_or_fail kv path >>= fun body ->
           S.respond_string ~status:`OK ~body ~headers ())
        (fun _exn ->
           S.respond_not_found ())
    | _ -> S.respond_not_found ()

  (* Redirect to the same address, but in https. *)
  let redirect port uri =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri (Some port) in
    Https_log.info (fun f -> f "[%s] -> [%s]"
                      (Uri.to_string uri) (Uri.to_string new_uri)
                  );
    let headers = Cohttp.Header.init_with "location" (Uri.to_string new_uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve dispatch =
    let callback (_, cid) request _body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Https_log.info (fun f -> f "[%s] serving %s." cid (Uri.to_string uri));
      dispatch uri
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Https_log.info (fun f -> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

end

module HTTPS
    (Pclock: Mirage_types_lwt.PCLOCK)
    (Data: Mirage_types_lwt.KV_RO)
    (Keys: Mirage_types_lwt.KV_RO)
    (Http: HTTP) =
struct

  module X509 = Tls_mirage.X509(Keys)(Pclock)
  module D = Dispatch(Data)(Http)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start _clock data keys http =
    tls_init keys >>= fun cfg ->
    let https_port = Key_gen.https_port () in
    let tls = `TLS (cfg, `TCP https_port) in
    http tls @@ D.serve (D.dispatcher data)
end
