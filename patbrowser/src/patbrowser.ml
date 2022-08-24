open Stitchy.Types
open Patbrowser

let dir =
  let doc = "directory from which to read." in
  Cmdliner.Arg.(value & pos 0 dir "." & info [] ~doc)

let start_state =
  let open Controls in
  let view = { x_off = 0;
               y_off = 0;
               block_display = `Symbol;
             }
  and selection = None
  in
  { mode = Browse;
    view;
    selection }

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

let insert_pattern traverse pattern tags =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
  let open Lwt.Infix in
  (* what's the filename? *)
  let filename = String.lowercase_ascii @@ Fpath.basename @@ List.nth traverse.View.contents traverse.n in
  let s = Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_string in
  Caqti_db.exec Db.ORM.Tags.insert tags.Controls.completed >>= function
  | Ok () ->
    Caqti_db.find Db.ORM.Patterns.insert_with_tags (filename, s, tags.Controls.completed)
  | Error _ as e -> Lwt.return e

let disp db dir =
  let open Lwt.Infix in
  let term = Notty_lwt.Term.create () in

  let rec aux dir traverse' state =
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
      View.main_view traverse db_info pattern state (Notty_lwt.Term.size term)
      >>= fun () ->

      let rec loop (pattern : pattern) (state : Controls.state) =
        (Lwt_stream.last_new user_input_stream) >>= fun event ->
          let size = Notty_lwt.Term.size term in
          match View.step state pattern size event with
          (* base case: let's bail *)
          | `Quit, _ -> Notty_lwt.Term.release term >>= fun () -> Lwt.return (Ok ())
          (* we should be able to further subselect stuff when in the preview *)
          | `Crop, state ->
            Notty_lwt.Term.image term (View.crop_then_view traverse db_info pattern state size) >>= fun () ->
            loop pattern state
          (*  `n` and `p` exit preview mode and go to the next/prior item,
           *  so we match on both Browse and Preview *)
          | `Prev, state -> aux dir {traverse with n = max 0 @@ traverse.n - 1; direction = Up} state (module Caqti_db)
          | `Next, state ->
            let next = 
              if traverse.n = List.length traverse.contents then traverse.n
              else traverse.n + 1
            in
            aux dir {traverse with n = next; direction = Down} state (module Caqti_db)
          | (`Tag, state) ->
            Notty_lwt.Term.image term (View.tag_view traverse db_info pattern state size) >>= fun () ->
            loop pattern state
          | (`Insert tags, _state) -> begin
            insert_pattern traverse pattern tags (module Caqti_db) >>= function
            | Ok id ->
              Notty_lwt.Term.image term (View.db_success size id) >>= fun () ->
              let state = start_state in
              loop pattern state
            | Error e ->
              let s = Format.asprintf "%a" Caqti_error.pp e in
              Notty_lwt.Term.image term (View.db_failure size s) >>= fun () ->
              let state = start_state in
              loop pattern state
          end
          | (`None, state) ->
            Notty_lwt.Term.image term (View.main_view traverse db_info pattern state size) >>= fun () ->
            loop pattern state
      in
      loop pattern start_state
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
          aux dir traverse start_state m
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
