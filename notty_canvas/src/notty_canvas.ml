open Stitchy.Types
open Canvas

let input =
  let doc = "file from which to read. -, the default, is stdin." in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let start_view = { Canvas__Controls.x_off = 0; y_off = 0; zoom = 1; block_display = `Symbol }
let disp input =
  let aux () =
    let start_state =
      try
        if 0 = String.compare input "-" then
          Yojson.Safe.from_channel stdin
        else
          Yojson.Safe.from_file input
      with _ -> failwith "couldn't read input file"
    in
    match start_state |> pattern_of_yojson with
    | Error e -> failwith (Printf.sprintf "failed to parse input json: %s" e)
    | Ok pattern ->
      let open Lwt.Infix in
      let term = Notty_lwt.Term.create () in
      let totals = Estimator.((materials pattern).threads |> totals) in
      Notty_lwt.Term.image term @@ main_view pattern start_view totals (Notty_lwt.Term.size term) >>= fun () ->
      let rec loop (pattern : pattern) (view : Canvas__Controls.view) =
        (Lwt_stream.last_new @@ Notty_lwt.Term.events term) >>= fun event ->
        let size = Notty_lwt.Term.size term in
        match step pattern view size event with
        | None -> Notty_lwt.Term.release term
        | Some (pattern, view) ->
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
