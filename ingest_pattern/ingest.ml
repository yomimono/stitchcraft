open Lwt.Infix

type db = { host : string;
            port : int;
            database : string;
            user : string;
            password : string;
          }

let set_db host port database user password = {host; port; database; user; password }

let db_t =
  let host =
    let doc = "postgresql server hostname" in
    let env = Cmdliner.Cmd.Env.info "PGHOST" in
    Cmdliner.Arg.(value & opt string "localhost" & info ["h"; "host"] ~docv:"PGHOST" ~doc ~env)
  in
  let port =
    let doc = "postgresql server port" in
    let env = Cmdliner.Cmd.Env.info "PGPORT" in
    Cmdliner.Arg.(value & opt int 5432 & info ["p"; "port"] ~docv:"PGPORT" ~doc ~env)
  in
  let database =
    let doc = "postgresql database name" in
    let env = Cmdliner.Cmd.Env.info "PGDATABASE" in
    Cmdliner.Arg.(value & opt string "stitchcraft" & info ["db"] ~docv:"PGDATABASE" ~doc ~env)
  in
  let user =
    let doc = "postgresql user" in
    let env = Cmdliner.Cmd.Env.info "PGUSER" in
    Cmdliner.Arg.(value & opt string "stitchcraft" & info ["u"] ~docv:"PGUSER" ~doc ~env)
  in
  let password =
    let doc = "postgresql user's password" in
    let env = Cmdliner.Cmd.Env.info "PGPASSWORD" in
    Cmdliner.Arg.(value & opt string "s3kr1t" & info ["pass"] ~docv:"PGPASSWORD" ~doc ~env)
  in
  Cmdliner.Term.(const set_db $ host $ port $ database $ user $ password)

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
  let query = {|
    CREATE TABLE IF NOT EXISTS patterns (
        id bigserial PRIMARY KEY,
        name text NOT NULL,
        pattern json NOT NULL,
        tags bigint[]
    )
    |}
  in
  Pgx_lwt_unix.execute_unit connection ~params:[] query

let create_tags connection =
  let query = {|
    CREATE TABLE IF NOT EXISTS tags (
        id bigserial PRIMARY KEY,
        name text UNIQUE NOT NULL
    )
    |}
  in
  Pgx_lwt_unix.execute_unit connection ~params:[] query

let ensure_tags connection tags =
  let query = {|
    INSERT INTO tags (name) VALUES ($1)
    ON CONFLICT DO NOTHING
    |} in
  let params = List.map (fun t -> Pgx.Value.[of_string t]) tags in
  Pgx_lwt_unix.execute_many connection ~params ~query >>= fun _rows ->
  Lwt.return_unit

let insert_pattern_with_tags connection pattern name tags =
  let pat_str = Yojson.Safe.to_string pattern in
  let params = Pgx.Value.[
      of_string name;
      of_string pat_str;
      of_list (List.map of_string tags)]
  in
  let query = {|
    INSERT INTO patterns (name, pattern, tags) 
    SELECT $1, $2, (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY ($3) ))
    |} in
  Pgx_lwt_unix.execute connection ~params query >>= fun _rows ->
  Lwt.return_unit

let tag_names = {|
    WITH unnested_tags AS (select unnest(tags) from patterns) select name from unnested_tags join tags on unnest = tags.id;
  |}

let search_patterns connection (tags : string list) =
  let query = {|
    SELECT id, name, pattern, tags
    FROM patterns
    WHERE tags @> (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY($1)))
    |}
  in
  let params = Pgx.Value.[of_list @@ List.map of_string tags] in
  Pgx_lwt_unix.execute connection ~params query >>= function
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

let go {host; port; database; user; password } file name tags =
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

let go_t = Cmdliner.Term.(const go $ db_t $ input $ p_name $ tags)
let info = Cmdliner.Cmd.info "ingest"

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info go_t
