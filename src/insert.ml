open Lwt.Infix

let search_patterns (module Caqti_db : Caqti_lwt.CONNECTION) (tags : string list) =
  Caqti_db.collect_list Db.ORM.Patterns.find tags >>= function
  | Ok ((id, name, _w, _h)::[]) ->
    Format.eprintf "pattern %d (%s) matches\n%!" id name;
    Lwt.return (Ok ())
  | Ok [] ->
    Format.eprintf "no matches\n%!";
    Lwt.return (Ok ())
  | Ok l ->
    Format.eprintf "lots of matches: %d\n%!" (List.length l);
    Lwt.return (Ok ())
  | Error _ as e -> Lwt.return e

let aux db file name tags =
  match Stitchy.Files.stdin_or_file file with
  | Error s -> Error s
  | Ok json ->
    match Stitchy.Types.pattern_of_yojson json with
    | Error s -> Error s
    | Ok pattern ->
      let open Lwt.Infix in
      Lwt_main.run (
        let uri = Db.CLI.uri_of_db db in
        Caqti_lwt.connect uri >>= function
        | Error _ as e -> Lwt.return e
        | Ok m ->
          let open Lwt_result.Infix in
          let module Caqti_db = (val m) in
          Caqti_db.exec Db.ORM.Patterns.create () >>= fun () ->
          Caqti_db.exec Db.ORM.Tags.create () >>= fun () ->
          Caqti_db.exec Db.ORM.Tags.insert tags >>= fun () ->
          let normalized_json = Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_string in
          Caqti_db.find Db.ORM.Patterns.insert_with_tags (name, normalized_json, tags) >>= fun id ->
          Format.printf "pattern inserted with id %d\n%!" id;
          search_patterns m tags >>= fun () ->
          Lwt.return @@ Ok ()
      ) |> function
      | Ok () -> Ok ()
      | Error (`Msg s) -> Error s
      | Error (#Caqti_error.t as e)-> Error (Format.asprintf "%a" Caqti_error.pp e)

let go db file name tags =
  aux db file name tags |> function
  | Ok () -> ()
  | Error s -> Format.eprintf "%s\n%!" s; exit 1
