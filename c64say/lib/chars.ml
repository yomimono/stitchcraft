module CharMap = Map.Make(Char)

(* some useful dimensions to have access to from a phrase *)
type dimensions = {
  lines : string list;
  height : int;
  width : int;
}

let h, w = 8, 8 (* c64 chars are 8x8 *)

let get_dimensions phrase interline =
  (* make the user figure out the line wrapping for us, for the moment. *)
  let lines = Astring.String.cuts ~sep:"\n" phrase in
  let height = (List.length lines) * h in
  let interline = (max 0 (List.length lines - 1)) * interline in
  let longest_line =
    List.fold_left (fun longest s ->
        max (Astring.String.length s) longest) 0 lines
  in
  let width = longest_line * w in
  { lines;
    height = height + interline;
    width }

let filename c =
  let open Astring.Char.Ascii in
  (* TODO: big hack here, this needs to work differently *)
  let base = "c64chars" in
  (* some special logic for PETSCII *)
  match is_alphanum c with
  | true when is_lower c ->
    Printf.sprintf "%s/%xl.png.layer0" base ((int_of_char c) - 0x20)
  | true when is_upper c ->
    Printf.sprintf "%s/%xu.png.layer0" base (int_of_char c)
  | _ ->
    Printf.sprintf "%s/%x.png.layer0" base (int_of_char c)

let load_layer file =
  try Ok (Yojson.Safe.from_file file |> Stitchy.Types.layer_of_yojson)
  with s -> Error s

let maybe_add c m =
  match load_layer (filename c) with
  | Error _ | Ok (Error _) ->
    Printf.eprintf "no layer loaded for character %x\n%!" (int_of_char c);
    m
  | Ok (Ok layer) -> CharMap.add c layer m

let map =
  let m = ref CharMap.empty in
  for c = 20 to 255 do
    m := maybe_add (Char.chr c) !m 
  done;
  Printf.printf "map has %d characters in it\n%!" @@ CharMap.cardinal !m;
  !m
