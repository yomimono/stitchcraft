open Stitchy.Types
open Patbrowser

let dir =
  let doc = "directory from which to read." in
  Cmdliner.Arg.(value & pos 0 dir "." & info [] ~doc)

let start_view = { Patbrowser.Controls.x_off = 0;
                   y_off = 0;
                   zoom = 1;
                   block_display = `Symbol;
                   selection = None;
                 }

let start_mode = Controls.Browse

let rec ingest dir traverse =
  let to_get =
    if traverse.View.n < 0 && traverse.direction = Down then 0
    else if traverse.n >= (List.length traverse.contents) && traverse.direction = Down then (List.length traverse.contents) - 1
    else traverse.n
  in
  match List.nth_opt traverse.contents to_get with
  | None -> Error "no more files to try"
  | Some filename ->
    let next_index =
      match traverse.direction with
      | Up -> to_get - 1
      | Down -> to_get + 1
    in
    try
      match Bos.OS.File.read filename with
      | Error _ ->
        (* are there additional files to try? *)
        ingest dir {traverse with n = to_get}
      | Ok contents ->
        match (Stitchy.Types.pattern_of_yojson @@ Yojson.Safe.from_string contents) with
        | Ok p -> Ok (p, traverse)
        | Error _ ->
          ingest dir {traverse with n = next_index}
    with
    | Unix.Unix_error _ -> ingest dir {traverse with n = next_index}
    | Yojson.Json_error _ -> ingest dir {traverse with n = next_index}

let find_filename_tags traverse =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
  let open Lwt.Infix in
  (* what's the filename? *)
  let filename = String.lowercase_ascii @@ Fpath.basename @@ List.nth traverse.View.contents traverse.n in
  (* is there a tag for it? *)
  Caqti_db.find Db.ORM.Tags.count [filename] >>= function
  | Error _ -> Lwt.return []
  | Ok 0 -> Lwt.return []
  | Ok _ ->
    (* do any patterns have the tag? *)
    Caqti_db.collect_list Db.ORM.Patterns.find [filename] >|= function
    | Error _ -> []
    | Ok l -> l

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
      find_filename_tags traverse (module Caqti_db) >>= fun filename_matches ->
      let db_info = {View.filename_matches; tags;} in
      (* show the initial view before waiting for events *)
      Notty_lwt.Term.image term @@
      View.main_view start_mode traverse db_info pattern view (Notty_lwt.Term.size term)
      >>= fun () ->

      let rec loop (mode : Controls.mode) (pattern : pattern) (view : Controls.view) =
        (Lwt_stream.last_new user_input_stream) >>= fun event ->
          let size = Notty_lwt.Term.size term in
          match mode, View.step mode pattern view size event with
          | _, (`Quit, _) -> Notty_lwt.Term.release term >>= fun () -> Lwt.return (Ok ())
          (* we should be able to further subselect stuff when in the preview *)
          | Browse, (`Crop, view) | Preview, (`Crop, view) ->
            let mode = Controls.Preview in
            Notty_lwt.Term.image term (View.crop_view mode traverse db_info pattern view size) >>= fun () ->
            loop mode pattern view
          (*  `n` and `p` exit preview mode and go to the next/prior item,
           *  so we match on both Browse and Preview *)
          | Browse, (`Prev, view) | Preview, (`Prev, view) -> aux dir {traverse with n = max 0 @@ traverse.n - 1; direction = Up} view (module Caqti_db)
          | Browse, (`Next, view) | Preview, (`Next, view) ->
            let next = 
              if traverse.n = List.length traverse.contents then traverse.n
              else traverse.n + 1
            in
            aux dir {traverse with n = next; direction = Down} view (module Caqti_db)
          | Tag _tags, (`Typing new_tags, view) ->
            Notty_lwt.Term.image term (View.tag_view new_tags view size) >>= fun () ->
            loop (Tag new_tags) pattern view
          | Tag _, (`Done _tags, view) ->
            (* TODO insert with tags *)
            aux dir traverse view (module Caqti_db)
          | _, (_, view) ->
            Notty_lwt.Term.image term (View.main_view mode traverse db_info pattern view size) >>= fun () ->
            loop mode pattern view
      in
      loop start_mode pattern view
  in
  (
    let open Rresult.R in
    Fpath.of_string dir >>= fun dir ->
    Bos.OS.Dir.contents dir >>= function
    | [] -> Error (`Msg "no patterns")
    | contents ->
      let contents = List.sort Fpath.compare contents in
      let traverse = {
        View.n = 0;
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
