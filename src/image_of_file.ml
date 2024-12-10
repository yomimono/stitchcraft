open Lwt

let lwt_read debug input_name output =
  Lwt_io.open_file ~mode:Input input_name >>= fun input ->
  if debug then Format.eprintf "successfully opened %s for reading\n%!" input_name else ();
  Lwt_io.read input >>= fun image ->
  match Imagereader.patternfy debug image with
  | Error _ -> Format.eprintf "error converting to pattern\n%!"; exit 1
  | Ok pattern ->
    match Util.stdout_or_file (Stitchy.Types.pattern_to_yojson pattern) output with
    | Ok () -> Lwt.return_unit
    | Error e -> Format.eprintf "error outputting: %s\n%!" e; exit 1

let read debug i o =
  Lwt_main.run @@ lwt_read debug i o
