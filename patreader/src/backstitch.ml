let eq_layers_disregarding_stitch_set layer stitch thread =
  Stitchy.Types.(equal_stitch layer.stitch stitch) &&
  Stitchy.DMC.Thread.compare layer.thread thread = 0

let add_stitches layers thread stitch new_stitches =
  match List.partition (fun layer ->
      eq_layers_disregarding_stitch_set layer stitch thread) layers with
  | layer::_, other_layers ->
    let stitches =
      Stitchy.Types.CoordinateSet.(union layer.stitches @@ of_list new_stitches) in
    Ok ({ layer with stitches } :: other_layers)
  | [], other_layers ->
    let layer = Stitchy.Types.{
        thread; stitch; stitches = CoordinateSet.of_list new_stitches
      } in
    Ok (layer :: other_layers)

(* the position numbers *appear* to be:
 1  2  3
 4  5  6
 7  8  9

 where 1, 3, 7, and 9 are the vertices of the grid lines
   and 5 is the center of the warp/weft overlap. *)

(* this means position number 3 on coordinate (1, 0)
 * is equivalent to position number 1 on coordinate (1, 1). *)
(* normalize_backstitch attempts to take any stitches which are just
 * one backstitch, but don't look like it because
 * they're expressed as the same position on different stitches,
 * and make them look like one backstitch.
 * to show this example visually: *)

(*
       0         1         2
   1   2   3/1---2---3/1   2   3

0  4   5   6/4   5   6/4   5   6

   7   8   9/7   8   9/7   8   9

*)

(* the stitch above is expressible as:
 * (0, 0):3 to (1, 0):3
 * (0, 0):3 to (2, 0):1
 * (1, 0):1 to (1, 0):3
 * (1, 0):1 to (2, 0):1
 
   We prefer the third because it's the only representation
   that maps nicely to the stitch grid we use for cross stitches,
   and this whole thing is supposed to be about those. Remember? *)

let normalize_backstitch backstitch =
  let open Patreader in
  match backstitch.start_position, backstitch.end_position with
  (* top-side backstitches *)
  | (3, 3) when (backstitch.end_x - backstitch.start_x) = 1 ->
    {backstitch with start_x = backstitch.end_x;
                     start_position = 1; }
  | (3, 1) when (backstitch.end_x - backstitch.start_x) = 2 ->
    {backstitch with start_x = backstitch.start_x + 1;
                     end_x = backstitch.end_x - 1;
                     start_position = 1;
                     end_position = 3; }
  | (1, 1) when (backstitch.end_x - backstitch.start_x) = 1 ->
    {backstitch with end_x = backstitch.start_x;
                     end_position = 3; }
(*
       0         1         2
   1   2   3/1   2   3/1   2   3

0  4   5   6/4   5   6/4   5   6

   7   8   9 7   8   9 7   8   9
   /   /    /    /    /    /   /
   1   2   3 1   2   3 1   2   3

1  4   5   6/4   5   6/4   5   6

   7   8   9/7   8   9/7   8   9
*)
  (* right-side backstitch *)
  | (9, 9) when (backstitch.end_y - backstitch.start_y) = 1 ->
    { backstitch with start_y = backstitch.start_y + 1;
                      start_position = 3; }
  | (9, 3) when (backstitch.end_y - backstitch.start_y) = 2 ->
    { backstitch with start_y = backstitch.start_y + 1;
                      end_y = backstitch.start_y - 1;
                      start_position = 3;
                      end_position = 9; }
  | (3, 3) when (backstitch.end_y - backstitch.start_y) = 1 ->
    { backstitch with end_y = backstitch.start_y;
                     end_position = 9; }
  (* left-side backstitch *)
  | (7, 7) when (backstitch.end_y - backstitch.start_y) = 1 ->
    { backstitch with start_y = backstitch.start_y + 1;
                      start_position = 1; }
  | (7, 1) when (backstitch.end_y - backstitch.start_y) = 2 ->
    { backstitch with start_y = backstitch.start_y + 1;
                      end_y = backstitch.start_y - 1;
                      start_position = 1;
                      end_position = 7; }
  | (1, 1) when (backstitch.end_y - backstitch.start_y) = 1 ->
    { backstitch with end_y = backstitch.start_y;
                     end_position = 7; }
  (* bottom-side backstitch *)
  | (9, 9) when (backstitch.end_x - backstitch.start_x) = 1 ->
    { backstitch with start_x = backstitch.start_x + 1;
                      start_position = 7; }
  | (7, 9) when (backstitch.end_x - backstitch.start_x) = 2 ->
    { backstitch with start_x = backstitch.start_x + 1;
                      end_x = backstitch.start_x - 1;
                      start_position = 7;
                      end_position = 9; }
  | (7, 7) when (backstitch.end_x - backstitch.start_x) = 1 ->
    { backstitch with end_x = backstitch.start_x;
                      end_position = 9; }
  | _ -> backstitch

let specific_stitches backstitch =
  let open Patreader in
  if backstitch.start_x = backstitch.end_x && backstitch.start_y = backstitch.end_y then begin
    match backstitch.start_position, backstitch.end_position with
    | (1, 3) | (3, 1) -> Ok (Stitchy.Types.Back Top, [(backstitch.start_x - 1, backstitch.start_y - 1)])
    | (1, 7) | (7, 1) -> Ok (Stitchy.Types.Back Left, [(backstitch.start_x - 1, backstitch.start_y - 1)])
    | (3, 9) | (9, 3) -> Ok (Stitchy.Types.Back Right, [(backstitch.start_x - 1, backstitch.start_y - 1)])
    | (7, 9) | (9, 7) -> Ok (Stitchy.Types.Back Bottom, [(backstitch.start_x - 1, backstitch.start_y - 1)])
    | _ -> Error (`Msg (Format.asprintf "unrepresentable stitch %a" pp_backstitch backstitch))
  end else Error (`Msg (Format.asprintf "unrepresentable stitch %a" pp_backstitch backstitch))

let rec break_backstitch acc backstitch =
  let backstitch = normalize_backstitch backstitch in
  (* degenerate case; a "backstitch" with only one point is nothing *)
  if backstitch.start_x = backstitch.end_x &&
     backstitch.start_y = backstitch.end_y && 
     backstitch.start_position = backstitch.end_position
  then acc
  (* base case; we can't break this down any further *)
  else if backstitch.start_x = backstitch.end_x &&
     backstitch.start_y = backstitch.end_y && 
     backstitch.start_position <> backstitch.end_position
  then specific_stitches backstitch :: acc
  (* a horizontal line of >1 cell *)
  else if backstitch.start_y = backstitch.end_y &&
          backstitch.start_x <> backstitch.end_x
  then begin

(*
       0         1         2
   1---2---3/1---2---3/1   2   3

0  4   5   6/4   5   6/4   5   6

   7   8   9/7   8   9/7   8   9
*)
    (* since the stitch has been normalized,
     * we can assume that the above is expressed as (0, 0):1 to (0, 1):3 *)
    let this_stitch = specific_stitches {backstitch with end_x = start_x + 1} in
    break_backstitch (this_stitch :: acc) {backstitch with start_x = start_x + 1}
  end
  (* a vertical line of >1 cell *)
  else if backstitch.start_x = backstitch.end_x &&
          backstitch.start_y <> backstitch.end_y
  then begin
    let this_stitch = specific_stitches {backstitch with end_y = start_y + 1} in
    break_backstitch (this_stitch :: acc) {backstitch with start_y = start_y + 1}
  end
  else begin
    let msg = Format.asprintf "unrepresentable, unbreakable backstitch: %a" pp_backstitch backstitch in
    Error `Msg msg::acc
  end

let layers_of_backstitches threads backstitches =
  let open Rresult.R in
  let one_backstitch acc backstitch =
    match acc with
    | Error e -> Error e
    | Ok layers ->
      match (List.nth threads (backstitch.Patreader.color_index - 1)) with
      | None -> Error (`Msg "unknown thread")
      | Some thread ->
        match List.fold_left break_backstitch [] backstitch with
        | Ok (stitch, stitches) ->
          Format.eprintf "interpreting backstitch %a as stitch %a for coords\n%a\n%!"
            Patreader.pp_backstitch backstitch
            Stitchy.Types.pp_stitch stitch
            Fmt.(list @@ parens @@ pair ~sep:Fmt.comma int int) stitches;
          add_stitches layers thread stitch stitches >>= fun layers ->
          Ok layers
        | Error (`Msg s) ->
          Format.eprintf "%s\n%!" s;
          acc
  in
  List.fold_left one_backstitch (Ok []) backstitches
