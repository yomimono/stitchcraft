open Stitchy.Types

let make_substrate background grid ~width ~height =
  { background;
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let max_dimensions (x1, y1) (x2, y2) =
        (max x1 x2), (max y1 y2)

let biggest_dims (layer : Stitchy.Types.layer) : (int * int) =
  (* TODO: now that stitches are a set, we could probably use the ordering
   * to do something smarter than folding over all the elements. *)
  CoordinateSet.fold max_dimensions layer.stitches (0, 0)

let find_size layers =
        List.fold_left (fun acc layer ->
      max_dimensions acc (biggest_dims layer)
    ) (0, 0) layers

(* If we happen to know what this thread is, get the Stitchy description for it
 * instead of relying on what the outputting program came up with.
 * This is helpful with `ih`, which attaches only identifiers to its threads. *)
let normalize_thread thread =
  (* TODO this is a brittle solution that relies on the thread module's to_string logic.
   * A better solution would have threads admit when they don't know who they are,
   * or allow for a replace_name function or something. *)
  match String.split_on_char ' ' @@ Stitchy.DMC.Thread.to_string thread with
  | _::identifier::_ -> begin
      let identifier = String.split_on_char ':' identifier |> List.hd
                       |> String.lowercase_ascii |> String.capitalize_ascii |> String.trim in
      match Stitchy.DMC.Thread.of_string identifier with
      | None -> thread
      | Some t -> t
    end
  | _ -> thread

(* humans usually like to supply "width, height" instead of
   "max_x", "max_y", so accommodate them preferentially. *)
let stitch background width height gridsize (layers : Stitchy.Types.layer list)=
  let max_x, max_y = find_size layers in
  let width = max (max_x + 1) width
  and height = max (max_y + 1) height
  in
  let substrate = make_substrate background gridsize ~width ~height in
  (* TODO: this is pokey enough that we should probably allow the user to bypass it *)
  let layers = List.fold_left (fun merged (layer : Stitchy.Types.layer) ->
      let normalized_layer = {layer with thread = normalize_thread layer.thread} in
      Stitchy.Layers.merge_threads merged [normalized_layer]
    ) [] layers in
  {layers; substrate; backstitch_layers = []}
