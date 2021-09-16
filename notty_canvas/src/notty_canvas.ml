open Stitchy.Types
open Canvas

let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let start_view = { Canvas__Controls.x_off = 0; y_off = 0; zoom = 1; block_display = `Symbol }
let disp input =
  let open Lwt.Infix in
  let initialize_pattern input =
    match String.compare input "-" with
    | 0 -> begin
        try Yojson.Safe.from_channel stdin, false
        with _ -> failwith "couldn't read input from stdin"
      end
    | _ ->
      try Yojson.Safe.from_file input, true
      with _ -> failwith "couldn't read input file"
  in
  let update_pattern input =
    try Yojson.Safe.from_file input |> pattern_of_yojson
    with _ -> failwith "reading updated file failed"
  in
  let aux () : unit Lwt.t =
    let term = Notty_lwt.Term.create () in
    Lwt_inotify.create () >>= fun inotify ->
    let user_input_stream = Lwt_stream.map
        (fun event -> `Terminal event)
        (Notty_lwt.Term.events term)
    in
    let create_streams = function
      | false -> Lwt.return user_input_stream
      | true ->
        Lwt_inotify.add_watch inotify input Inotify.([S_Modify;S_Create;S_Close_write]) >>= fun _watch ->
        let file_watch_stream = Lwt_stream.map
            (fun _ -> `Pattern)
            (Lwt_stream.from @@ 
             fun () -> Lwt_inotify.read inotify >|= fun e -> Some e)
        in
        Lwt.return @@
        Lwt_stream.choose [
          user_input_stream;
          file_watch_stream;
        ]
    in
    let (start_state, watch_input) = initialize_pattern input in
    match start_state |> pattern_of_yojson with
    | Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
    | Ok pattern ->
      (* we don't care about the fabric part of the estimate,
       * so it's OK to pass 0 for margin inches here *)
      let totals = Estimator.((materials ~margin_inches:0.
                                 pattern).threads |> totals) in
      Notty_lwt.Term.image term @@ main_view pattern start_view totals (Notty_lwt.Term.size term) >>= fun () ->
      create_streams watch_input >>= fun stream ->
      let rec loop (pattern : pattern) (view : Canvas__Controls.view) =
        (Lwt_stream.last_new stream) >>= function
        | `Pattern -> begin
            match update_pattern input with
            | Error _ -> loop pattern view
            | Ok pattern ->
              let size = Notty_lwt.Term.size term in
              let totals = Estimator.((materials ~margin_inches:0.
                                         pattern).threads |> totals) in
              Notty_lwt.Term.image term (main_view pattern view totals size) >>= fun () ->
              loop pattern view
          end
        | `Terminal event ->
          let size = Notty_lwt.Term.size term in
          match step pattern view size event with
          | None ->
            Notty_lwt.Term.release term >>= fun () ->
            Lwt_inotify.close inotify
          | Some (refresh_pattern, view) ->
            let totals = Estimator.((materials ~margin_inches:0.
                                       pattern).threads |> totals) in
            let pattern =
              if refresh_pattern then begin
                match update_pattern input with
                | Ok new_pattern -> new_pattern
                | Error _ -> pattern
              end
              else pattern
            in
            Notty_lwt.Term.image term (main_view pattern view totals size) >>= fun () ->
            loop pattern view
      in
      loop pattern start_view
  in
  Lwt_main.run @@ aux ()

let info =
  let doc = "display/explore a cross-stitch pattern on the terminal" in
  Cmdliner.Term.info "notty_canvas" ~doc

let disp_t = Cmdliner.Term.(const disp $ input)

let () =
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (disp_t, info)
