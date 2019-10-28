open Mirage

(* "static" stuff for the web server - certs, client code *)
let data_key = Key.(value @@ kv_ro ~group:"data" ())
let data = generic_kv_ro ~key:data_key "client-fs"

let certs_key = Key.(value @@ kv_ro ~group:"certs" ())
let certs = generic_kv_ro ~key:certs_key "tls"

(* networking *)
let stack = generic_stackv4 default_network
let conduit_tls = conduit_direct ~tls:true stack
let https_srv = cohttp_server conduit_tls

let https_port =
  let doc = Key.Arg.info ~doc:"Listening HTTPS port." ["https"] in
  Key.(create "https_port" Arg.(opt int 443 doc))

let main =
  let packages = [
  ] in
  let keys = [Key.abstract https_port] in
  foreign
    ~packages ~keys
    "Dispatch.HTTPS" (pclock @->
                      kv_ro @-> (* static data *)
                      kv_ro @-> (* certs *)
                      http @-> (* server for static/dynamic stuff *)
                      job)

let () =
  register "https" [main $ default_posix_clock $
                    data $ certs $
                    https_srv ]
