open Stitchy.Types

module View = Patbrowser.View
module Controls = Patbrowser.Controls

let start_state =
  let open Patbrowser.Controls in
  let view = { x_off = 0;
               y_off = 0;
               block_display = `Solid;
             }
  and selection = None
  in
  { mode = Browse;
    view;
    selection }

let rec ingest dir traverse =
  let open Patbrowser.View in
  let to_get =
    if traverse.n < 0 && traverse.direction = Down then 0
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
        ingest dir {traverse with n = next_index}
      | Ok contents ->
        match (Stitchy.Types.pattern_of_yojson @@ Yojson.Safe.from_string contents) with
        | Ok p -> Ok (p, traverse)
        | Error _ ->
          ingest dir {traverse with n = next_index}
    with
    | Unix.Unix_error _ when next_index < List.length traverse.contents ->
      ingest dir {traverse with n = next_index}
    | Yojson.Json_error _ when next_index < List.length traverse.contents ->
      ingest dir {traverse with n = next_index}
    | _ -> Error "no more files to try"

let save_pattern pattern output_filename =
  Yojson.Safe.to_file output_filename @@ Stitchy.Types.pattern_to_yojson pattern

let disp dir =
  let open Lwt.Infix in
  let term = Notty_lwt.Term.create () in

  let rec aux dir traverse' state =
    let user_input_stream = Notty_lwt.Term.events term in
    match ingest dir traverse' with
    | Error s ->
      Notty_lwt.Term.release term >>= fun () ->
      Lwt.return @@ Error (`Msg s)
    | Ok (pattern, traverse) ->
      (* show the initial view before waiting for events *)
      Notty_lwt.Term.image term @@
      View.main_view traverse pattern state (Notty_lwt.Term.size term)
      >>= fun () ->
      let rec loop (pattern : pattern) (state : Controls.state) =
        (Lwt_stream.last_new user_input_stream) >>= fun event ->
          let size = Notty_lwt.Term.size term in
          match View.step state pattern size event with
          (* base case: let's bail *)
          | `Quit, _ -> Notty_lwt.Term.release term >>= fun () -> Lwt.return (Ok ())
          (* we should be able to further subselect stuff when in the preview *)
          | `Crop, state -> begin
            match View.crop pattern state with
            | None ->
              Notty_lwt.Term.image term (View.failure size "crop failed! check for backstitch") >>= fun () ->
              loop pattern state
            | Some subpattern ->
              let state = {state with mode = Preview; selection = None} in
              Notty_lwt.Term.image term (View.main_view traverse subpattern state size) >>= fun () ->
              loop subpattern state
          end
          (*  `n` and `p` exit preview mode and go to the next/prior item,
           *  so we match on both Browse and Preview *)
          | `Prev, state -> aux dir {traverse with n = max 0 @@ traverse.n - 1; direction = Up} state
          | `Next, state ->
            let next = 
              if traverse.n = List.length traverse.contents then traverse.n
              else traverse.n + 1
            in
            aux dir {traverse with n = next; direction = Down} state
          | (`None, state) ->
            Notty_lwt.Term.image term (View.main_view traverse pattern state size) >>= fun () ->
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
      Lwt_main.run (aux dir traverse start_state)
  ) |> function
  | Ok () -> ()
  | Error (`Msg s) -> Format.eprintf "error: %s\n%!" s; exit 1
