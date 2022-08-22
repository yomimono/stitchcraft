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
  match List.nth_opt traverse.contents traverse.n with
  | None -> Error "no more files to try"
  | Some filename ->
    try
      match Bos.OS.File.read filename with
      | Error _ ->
        (* are there additional files to try? *)
        ingest dir {traverse with n = traverse.n + 1}
      | Ok contents ->
        (try
           Ok (Yojson.Safe.from_string contents)
         with _ -> Error "not json")
        |> function
        | Ok j -> Stitchy.Types.pattern_of_yojson j
        | Error _ as e -> e
    with
    | Unix.Unix_error _ -> ingest dir {traverse with n = traverse.n + 1}

let disp db dir =
  let open Lwt.Infix in
  let term = Notty_lwt.Term.create () in
  let rec aux dir traverse view =
    fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
    Format.eprintf "hello from aux, everything is great, how are you\n%!";
    Format.eprintf "hello from aux, everything is great, how are you\n%!";
    Format.eprintf "hello from aux, everything is great, how are you\n%!";
    let user_input_stream = Notty_lwt.Term.events term in
    match ingest dir traverse with
    | Error s -> Format.eprintf "error: %s\n%!" s; Notty_lwt.Term.release term
    | Ok pattern ->
      Notty_lwt.Term.image term @@ main_view traverse pattern view (Notty_lwt.Term.size term) >>= fun () ->
      let rec loop (pattern : pattern) (view : Patbrowser_canvas__Controls.view) =
        (Lwt_stream.last_new user_input_stream) >>= fun event ->
          let size = Notty_lwt.Term.size term in
          match step pattern view size event with
          | `Quit, _ -> Notty_lwt.Term.release term
          | `None, view ->
            Notty_lwt.Term.image term (main_view traverse pattern view size) >>= fun () ->
            loop pattern view
          | `Next, view -> aux dir {traverse with n = traverse.n + 1} view (module Caqti_db)
          | `Prev, view -> aux dir {traverse with n = max 0 @@ traverse.n - 1} view (module Caqti_db)
      in
      loop pattern view
  in
  (
    let open Rresult.R in
    Format.eprintf "hey yall its ya boi\n%!";
    Fpath.of_string dir >>= fun dir ->
    Bos.OS.Dir.contents dir >>= function
    | [] -> Error (`Msg "no patterns")
    | contents ->
      Format.eprintf "comin' at y'all with a brand new sound\n%!";
      let contents = List.sort Fpath.compare contents in
      let traverse = {
        n = 0;
        contents;
      } in
      Format.eprintf "I got %d patterns to show to you\n%!" @@ List.length contents;
      Lwt_main.run (
        Lwt.catch (fun () ->
            let open Lwt.Infix in
            Format.eprintf "and now I'm gonna take the mound\n%!";
            let uri = Db.CLI.uri_of_db db in
            Format.eprintf "uri get\n%!";
            Caqti_lwt.connect uri >>= fun thing ->
            Format.eprintf "our connection attempt finished\n%!";
            match thing with
            | Error e ->
              Format.eprintf "error connecting to database: %a\n%!" Caqti_error.pp e;
              exit 1
            | Ok m ->
              Format.eprintf "uh sure ok\n%!";
              Format.eprintf "connected\n%!";
              aux dir traverse start_view m >>= fun () ->
              Format.eprintf "I did it!!!!\n%!";
              Lwt.return_unit
          )
          (
            fun _ -> Format.eprintf "fuck\n%!"; Lwt.return_unit
          )
      ) |>
      fun () ->
      Format.eprintf "whomst among us\n%!";
      Ok ()
  ) |> function
  | Ok () -> Format.eprintf "ok im done\n%!"; Ok ()
  | Error (`Msg s) -> Format.eprintf "%s\n%!" s; Error s

let info =
  let doc = "slice, dice, and tag patterns" in
  Cmdliner.Cmd.info "patbrowser" ~doc

let disp_t = Cmdliner.Term.(const disp $ Db.CLI.db_t $ dir)

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info disp_t
