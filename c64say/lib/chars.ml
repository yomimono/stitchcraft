let find_char font_name =
  Caqti_request.find_opt Caqti_type.int Caqti_type.string @@
    "SELECT glyph FROM " ^ font_name ^ " WHERE uchar = ?"

let load_layer db_module font c =
  let open Lwt.Infix in
  let module Db = (val db_module : Caqti_lwt.CONNECTION) in
  Db.find_opt (find_char font) (Uchar.to_int c) >|= function
  | Error e -> Error (Format.asprintf "Error looking up %x: %a" (Uchar.to_int c) Caqti_error.pp e)
  | Ok None -> Error (Format.asprintf "No result for %x" (Uchar.to_int c))
  | Ok (Some s) ->
    try (Yojson.Safe.from_string s |> Stitchy.Types.glyph_of_yojson)
    with _ -> Error "JSON deserialization error or glyph reconstruction error"

let maybe_add db_module font c m =
  let open Lwt.Infix in
  load_layer db_module font c >|= function
  | Ok layer -> Stitchy.Types.UcharMap.add c layer m
  | Error _ -> m

let map db_module font =
  let open Lwt.Infix in
  let m = ref Stitchy.Types.UcharMap.empty in
  let fetch_char c = maybe_add db_module font (Uchar.of_int c) !m in
  let minimum_printable = 0x20 in
  let chars = List.init (255-minimum_printable) (fun c -> minimum_printable + c) in
  (* get the characters in base ASCII *)
  Lwt_list.iter_s
    (fun c ->
       fetch_char c >>= fun new_map ->
       m := new_map;
       Lwt.return_unit
    ) chars >>= fun () ->
  Lwt.return !m
