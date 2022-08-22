open Stitchy.Types
open Patbrowser_canvas

let dir =
  let doc = "directory from which to read." in
  Cmdliner.Arg.(value & pos 0 dir "." & info [] ~doc)

let start_view = { Patbrowser_canvas__Controls.x_off = 0;
                   y_off = 0;
                   zoom = 1;
                   block_display = `Symbol;
                   selection = None;
                 }

let rec ingest dir traverse =
  let next_index = match traverse.direction with
    | Up -> max 0 @@ traverse.n - 1
    | Down -> min ((List.length traverse.contents) - 1) @@ traverse.n + 1
  in
  match List.nth_opt traverse.contents traverse.n with
  | None -> Error "no more files to try"
  | Some filename ->
    try
      match Bos.OS.File.read filename with
      | Error _ ->
        (* are there additional files to try? *)
        ingest dir {traverse with n = next_index}
      | Ok contents ->
        match (Stitchy.Types.pattern_of_yojson @@ Yojson.Safe.from_string contents) with
        | Ok p -> Ok (p, traverse)
        | Error _ ->
          ingest dir {traverse with n = next_index}
    with
    | Unix.Unix_error _ -> ingest dir {traverse with n = next_index}
    | Yojson.Json_error _ -> ingest dir {traverse with n = next_index}

let disp db dir =
  let open Lwt.Infix in
  let term = Notty_lwt.Term.create () in
  let rec aux dir traverse' view =
    fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
    let user_input_stream = Notty_lwt.Term.events term in
    match ingest dir traverse' with
    | Error s ->
      Notty_lwt.Term.release term >>= fun () ->
      Lwt.return @@ Error (`Msg s)
    | Ok (pattern, traverse) ->
      (Caqti_db.collect_list Db.ORM.Tags.all_names () >|= function
      | Error _ -> []
      | Ok tags -> tags
      ) >>= fun tags ->
      let db_info = {filename_matches = []; tags;} in
      Notty_lwt.Term.image term @@ main_view traverse db_info pattern view (Notty_lwt.Term.size term) >>= fun () ->
      let rec loop (pattern : pattern) (view : Patbrowser_canvas__Controls.view) =
        (Lwt_stream.last_new user_input_stream) >>= fun event ->
          let size = Notty_lwt.Term.size term in
          match step pattern view size event with
          | `Quit, _ -> Notty_lwt.Term.release term >>= fun () -> Lwt.return (Ok ())
          | `None, view ->
            Notty_lwt.Term.image term (main_view traverse db_info pattern view size) >>= fun () ->
            loop pattern view
          | `Prev, view -> aux dir {traverse with n = max 0 @@ traverse.n - 1; direction = Up} view (module Caqti_db)
          | `Next, view ->
            let next = 
              if traverse.n = List.length traverse.contents then traverse.n
              else traverse.n + 1
            in
            aux dir {traverse with n = next; direction = Down} view (module Caqti_db)
      in
      loop pattern view
  in
  (
    let open Rresult.R in
    Fpath.of_string dir >>= fun dir ->
    Bos.OS.Dir.contents dir >>= function
    | [] -> Error (`Msg "no patterns")
    | contents ->
      let contents = List.sort Fpath.compare contents in
      let traverse = {
        n = 0;
        contents;
        direction = Down;
      } in
      Lwt_main.run (
        let open Lwt.Infix in
        Caqti_lwt.connect (Db.CLI.uri_of_db db) >>= function
        | Error e -> Lwt.return (Error (`Msg 
          (Format.asprintf "error connecting to database: %a\n%!" Caqti_error.pp e)
                                     ))
        | Ok m ->
          aux dir traverse start_view m
      )
  ) |> function
  | Ok () -> Ok ()
  | Error (`Msg s) -> Format.eprintf "%s\n%!" s; Error s

let info =
  let doc = "slice, dice, and tag patterns" in
  Cmdliner.Cmd.info "patbrowser" ~doc

let disp_t = Cmdliner.Term.(const disp $ Db.CLI.db_t $ dir)

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info disp_t
