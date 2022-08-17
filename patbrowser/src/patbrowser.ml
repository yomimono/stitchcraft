open Stitchy.Types
open Patbrowser_canvas

let dir =
  let doc = "directory from which to read." in
  Cmdliner.Arg.(value & pos 0 string "." & info [] ~doc)

let start_view = { Patbrowser_canvas__Controls.x_off = 0; y_off = 0; zoom = 1; block_display = `Symbol }

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
    let user_input_stream = Lwt_stream.map
        (fun event -> `Terminal event)
        (Notty_lwt.Term.events term)
    in
    let create_streams = function
      | _ -> Lwt.return user_input_stream
    in
    let (start_state, watch_input) = initialize_pattern input in
    match start_state |> pattern_of_yojson with
    | Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
    | Ok pattern ->
      (* we don't care about the fabric part of the estimate,
       * so it's OK to pass 0 for margin inches here *)
      let thread_totals = Estimator.((materials ~margin_inches:0.
                                 pattern).threads |> thread_totals) in
      Notty_lwt.Term.image term @@ main_view pattern start_view thread_totals (Notty_lwt.Term.size term) >>= fun () ->
      create_streams watch_input >>= fun stream ->
      let rec loop (pattern : pattern) (view : Patbrowser_canvas__Controls.view) =
        (Lwt_stream.last_new stream) >>= function
        | `Pattern -> begin
            match update_pattern input with
            | Error _ -> loop pattern view
            | Ok pattern ->
              let size = Notty_lwt.Term.size term in
              let thread_totals = Estimator.((materials ~margin_inches:0.
                                 pattern).threads |> thread_totals) in
              Notty_lwt.Term.image term (main_view pattern view thread_totals size) >>= fun () ->
              loop pattern view
          end
        | `Terminal event ->
          let size = Notty_lwt.Term.size term in
          match step pattern view size event with
          | None ->
            Notty_lwt.Term.release term
          | Some (refresh_pattern, view) ->
            let thread_totals = Estimator.((materials ~margin_inches:0.
                                 pattern).threads |> thread_totals) in
            let pattern =
              if refresh_pattern then begin
                match update_pattern input with
                | Ok new_pattern -> new_pattern
                | Error _ -> pattern
              end
              else pattern
            in
            Notty_lwt.Term.image term (main_view pattern view thread_totals size) >>= fun () ->
            loop pattern view
      in
      loop pattern start_view
  in
  Lwt_main.run @@ aux ()

let info =
  let doc = "slice, dice, and tag patterns" in
  Cmdliner.Cmd.info "patbrowser" ~doc

let disp_t = Cmdliner.Term.(const disp $ dir)

let () =
  exit @@ Cmdliner.Cmd.eval @@ Cmdliner.Cmd.v info disp_t
