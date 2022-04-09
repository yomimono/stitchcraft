open Lwt.Infix

let check_signature _request =
  (* TODO: stripe signature verification logic *)
  false

let try_fulfill request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  match check_signature request with
  | false -> Dream.respond ~code:400 ""
  | true -> Dream.respond ~code:500 ""

let () =
  Dream.(run @@ logger @@ memory_sessions @@ sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ router [
      Dream.post "/fulfill" (fun request -> Dream.sql request @@ try_fulfill request)

  ])
