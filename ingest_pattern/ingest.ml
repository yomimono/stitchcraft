open Lwt.Infix

let p_name =
  let doc = "name to associate with the pattern" in
  Cmdliner.Arg.(value & pos 0 string "My Very Excellent Mother Just Served Us Nine Patterns!" & info [] ~doc ~docv:"NAME")

let input = 
  let doc = "file to read for pattern ingestion. -, the default, is stdin" in
  Cmdliner.Arg.(value & opt string "-" & info ["i"; "input"] ~doc ~docv:"INPUT")

let tags =
  let doc = "tags to associate with the pattern" in
  Cmdliner.Arg.(value & opt_all string [] & info ["t"; "tag"] ~doc ~docv:"TAG")

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

let go db file name tags =
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

let go_t = Cmdliner.Term.(const go $ Db.CLI.db_t $ input $ p_name $ tags)
let info = Cmdliner.Cmd.info "ingest"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info go_t
