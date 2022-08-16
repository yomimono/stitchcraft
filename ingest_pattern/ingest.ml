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

let create_patterns connection =
  Pgx_lwt_unix.execute_unit connection ~params:[] Db.ORM.Patterns.create

let create_tags connection =
  Pgx_lwt_unix.execute_unit connection ~params:[] Db.ORM.Tags.create

let ensure_tags connection tags =
  let params = List.map (fun t -> Pgx.Value.[of_string t]) tags in
  Pgx_lwt_unix.execute_many connection ~params ~query:Db.ORM.Tags.insert >>= fun _rows ->
  Lwt.return_unit

let insert_pattern_with_tags connection pattern name tags =
  let pat_str = Yojson.Safe.to_string pattern in
  let params = Pgx.Value.[
      of_string name;
      of_string pat_str;
      of_list (List.map of_string tags)]
  in
  Pgx_lwt_unix.execute connection ~params Db.ORM.Patterns.insert >>= fun _rows ->
  Lwt.return_unit

let search_patterns connection (tags : string list) =
  let params = Pgx.Value.[of_list @@ List.map of_string tags] in
  Pgx_lwt_unix.execute connection ~params Db.ORM.Patterns.search_by_tag >>= function
  | (i::n::_p::_t)::_ ->
    let id = Pgx.Value.to_int_exn i
    and name = Pgx.Value.to_string_exn n
    in
    Format.eprintf "pattern %d (%s) matches\n%!" id name;
    Lwt.return_unit
  | [] ->
    Format.eprintf "no matches\n%!";
    Lwt.return_unit
  | l ->
    Format.eprintf "lots of matches: %d\n%!" (List.length l);
    Lwt.return_unit

let go {Db.CLI.host; port; database; user; password } file name tags =
  match Stitchy.Files.stdin_or_file file with
  | Error s -> Error s
  | Ok pattern ->
    Lwt_main.run (
      Pgx_lwt_unix.connect ~host ~port ~database ~user ~password () >>= fun connection ->
      create_patterns connection >>= fun () ->
      create_tags connection >>= fun () ->
      ensure_tags connection tags >>= fun () ->
      insert_pattern_with_tags connection pattern name tags >>= fun _ ->
      search_patterns connection tags >>= fun () ->
      Lwt.return @@ Ok ()
    )

let go_t = Cmdliner.Term.(const go $ Db.CLI.db_t $ input $ p_name $ tags)
let info = Cmdliner.Cmd.info "ingest"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info go_t
