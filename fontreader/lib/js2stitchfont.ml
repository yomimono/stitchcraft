open Angstrom

(* this is a parser for a JavaScript array of Unicode character -> string lists.
 * Each string list represents the segments of a 14-segment display which are
 * active to represent that glyph.
 *
 * See https://35007.de/chargenerator.htm for a clearer example and source data. *)

type glyphmap = (Stitchy.Types.glyph * Uchar.t list) list

type error = string

let pp_error = Fmt.string

let empty_glyph = Stitchy.Types.({
  height = 3;
  width = 3;
  backstitches = Stitchy.Types.SegmentSet.empty;
  stitches = Stitchy.Types.CoordinateSet.empty;
})

let segments_of_seg_labels = function
  | "A1" -> ((0, 0), (1, 0))
  | "A2" -> ((1, 0), (2, 0))
  | "F"  -> ((0, 0), (0, 1))
  | "H"  -> ((0, 0), (1, 1))
  | "I"  -> ((1, 0), (1, 1))
  | "J"  -> ((1, 1), (2, 0))
  | "B"  -> ((2, 0), (2, 1))
  | "G1" -> ((0, 1), (1, 1))
  | "G2" -> ((1, 1), (2, 1))
  | "E"  -> ((0, 1), (0, 2))
  | "K"  -> ((1, 1), (0, 2))
  | "L"  -> ((1, 1), (1, 2))
  | "M"  -> ((1, 1), (2, 2))
  | "C"  -> ((2, 1), (2, 2))
  | "D1" -> ((0, 2), (1, 2))
  | "D2" -> ((1, 2), (2, 2))
(* "DP" also exists, which is a dot hanging out in the bottom right;
 * it's used as a period, the dot on an exclamation point, and a few
 * others.
 *
 * It's obviously supposed to be a french knot, so I'm ignoring it. *)
  | s -> failwith @@ "Unknown segment label " ^ s

let uchar_of_string s =
  try
    int_of_string s |> Uchar.of_int
  with Invalid_argument _ -> failwith (s ^ " is not a uchar")

let single_quoted =
  char '\'' >>= fun _ ->
  take_till (Char.equal '\'') >>= fun s ->
  char '\'' >>= fun _ ->
  return s

let line _debug =
  char 'U' >>= fun _ ->
  take_till (Char.equal ':') >>= fun amt ->
  char ':' >>= fun _ ->
  take_till (Char.equal '[') >>= fun _ ->
  char '[' >>= fun _ ->
  sep_by (char ',') single_quoted >>= fun seg_labels ->
  char ']' >>= fun _ ->
  skip_while (fun c -> not @@ Char.equal '\n' c) >>= fun () ->
  end_of_line >>= fun () ->
  let hex = "0x" ^ amt in
  let u = uchar_of_string hex in
  let valid_segments = List.filter (fun s -> not @@ String.equal "DP" s) seg_labels in
  let segments = List.map segments_of_seg_labels valid_segments in
  let backstitches = Stitchy.Types.SegmentSet.of_list segments in
  return ({ empty_glyph with backstitches = backstitches}, [u])

let js debug =
  let open Angstrom in
  string "var charset = {" >>= fun _s ->
  end_of_line >>= fun _ ->
  many (line debug)

let glyphmap_of_buffer debug cs =
  let bs = Cstruct.to_bigarray cs in
  Angstrom.parse_bigstring ~consume:Prefix (js debug) bs
