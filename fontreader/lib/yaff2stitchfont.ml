type glyphmap = (Stitchy.Types.glyph * Uchar.t list) list
type error = string

open Angstrom

(* TODO: there is per-glyph metadata (and per-font metadata) about placement
 * which we should handle. For an example, this glyph from hoard-of-bitfonts
 * apple/mac/Venice_14.yaff :
 *
 *

u+0021:
0x21:
    ..
    ..
    ..
    ..
    ..
    @@
    @@
    @@
    @@
    @@
    @@
    @@
    ..
    @@
    @@
    ..
    ..
    ..
    ..

    left-bearing: 2
    right-bearing: 1

   currently, seeing something like this stops the parse entirely
*)

let pp_error fmt s = Format.fprintf fmt "%s" s

let metadata =
  let trim = Astring.String.trim in
  take_till (Char.equal ':') >>= fun label ->
  char ':' >>= fun _ ->
  take_till (Char.equal '\n') >>= fun value ->
  end_of_line >>= fun _ ->
  if String.length value < 1 then fail "no more"
  else return ((trim label), (trim value))

let eight_bit debug =
  string "0x" >>= fun _ ->
  if debug then Format.eprintf "looks like an eight-bit label\n%!";
  take_till (Char.equal ':') >>= fun s ->
  (* we need to reappend this so int_of_string knows what to do with hex *)
  let hex = "0x" ^ s in
  (* these are "plain" 8-bit chars, so we run them through Char.chr *)
  match int_of_string_opt hex with
  | Some n when n < 0x100 -> return (Char.chr n |> Uchar.of_char)
  | _ -> Angstrom.fail "not mapping ASCII characters over 0xff"

(* currently not handled: there can be multiple code points in a label, which define a "grapheme cluster" -
 * it's not clear to me how we'd even express that in the target data structure, unless that "grapheme cluster"
 * happens to normalize to some other code point. *)
(* as it stands now, we will fail to parse the glyph if we encounter such a label. *)
let unicode debug =
  (* unicode values get to go directly to Uchar.of_int *)
  let uchar_of_string s = int_of_string s |> Uchar.of_int in
  string "u+" >>= fun _ ->
  if debug then Format.eprintf "looks like a unicode label\n%!";
  take_till (Char.equal ':') >>| fun s ->
  if debug then Format.eprintf "hex string: %s\n%!" s;
  (* we need to reappend 0x so int_of_string knows what to do with hex *)
  uchar_of_string ("0x" ^ s)

let missing = string "missing:\n"

let glyph_label debug  =
  (unicode debug <|> eight_bit debug) >>= fun res ->
  if debug then Format.eprintf "glyph labels %a\n%!" Fmt.(int) (Uchar.to_int res);
  char ':' >>= fun _ -> end_of_line >>= fun () -> return res

let maybe_stitch =
  let stitch_of_char = function
    | '.' -> `Blank
    | _ -> `Stitch
  in
  satisfy (fun c -> not @@ Char.equal '\n' c) >>| stitch_of_char

let line_to_coordinates ~(y : int) xs =
  let max_x, stitches =
    List.fold_left (fun (x, stitches) stitch -> match stitch with
      | `Blank -> (x+1, stitches)
      | `Stitch -> (x+1, Stitchy.Types.CoordinateSet.add (x, y) stitches)
    ) (0, Stitchy.Types.CoordinateSet.empty) xs
  in
  (max_x, stitches)

let lines_to_glyph lines =
  let max_y, max_x, stitches =
    List.fold_left (fun (y, _max_x, stitches) line ->
        let max_x, new_stitches = line_to_coordinates ~y line in
        (y+1, max_x,
         Stitchy.Types.CoordinateSet.union new_stitches stitches)
      ) (0, 0, Stitchy.Types.CoordinateSet.empty) lines
  in
  (* it seems we called it "width" and "height" in glyph, but it's really
   * max_x, max_y *)
  { Stitchy.Types.width = max_x;
    height = max_y;
    stitches;
    backstitches = Stitchy.Types.SegmentSet.empty;
  }

let stitch_line debug =
  take_while (Char.equal ' ') >>= fun _blank ->
  many1 maybe_stitch >>= fun xs ->
  end_of_line >>= fun () ->
  let () =
    if debug then Format.eprintf "got a line of %d stitches\n%!" (List.length xs)
    else ()
  in
  return xs

let bitmap debug =
  peek_string 10 >>= fun s ->
  if debug then Format.eprintf "looking for a bitmap starting at %S\n%!" s;
  many1 (stitch_line debug) >>| fun lines ->
  if debug then Format.eprintf "got %d lines, trying to make them a glyph\n%!" (List.length lines);
  lines_to_glyph lines

let normalize (glyph : Stitchy.Types.glyph) ~font_metadata ~glyph_metadata =
  let open Stitchy.Types in 
  let altered_opt k =
    match List.assoc_opt k font_metadata, List.assoc_opt k glyph_metadata with
    | None, None -> None
    | Some v, None | None, Some v -> int_of_string_opt v
    | Some font_v, Some glyph_v ->
      match int_of_string_opt font_v, int_of_string_opt glyph_v with
      | None, None -> None
      | Some v, None | None, Some v -> Some v
      | Some fv, Some gv -> Some (fv + gv)
  in
  let altered k =
    match altered_opt k with
    | None -> 0
    | Some n -> n
  in
  (* ok, this is potentially fairly gnarly. *)
  (* it's probably possible to take the metadata pairs and construct some kind of metrics object,
   * but it's going to be so option-y that I think that'll be pretty annoying to deal with.
   *)
  (* many 'metrics' (keys in the font_metadata and glyph_metadata associative lists) are present
   * on both the font level and the glyph level,
   * and if present in both, the values (which are supposed to be integers) are summed. *)
  let leftness = altered "left-bearing" in
  let rightness = altered "right-bearing" in
  let upness = altered "shift-up" in
  (* any of these values might be *negative*, which means there is space in the
   * glyph already which we should ignore - in other words, slice bits off the glyph in that direction *)
  (* I imagine this is to handle ascenders and descenders nicely,
   * and is probably going to result in all kinds of fun chaos in our own typesettings *)
  (* if it's positive, we need to add empty space -
   * in the case of rightness or upness, that can simply be
   * enlarging the substrate,
   * but for leftness, we need to transpose the coordinates rightward *)
  let leftify glyph =
    let coordinates = Stitchy.Types.CoordinateSet.map (fun (x, y) -> (x + leftness, y)) glyph.stitches in
    { glyph with stitches = coordinates; width = glyph.width + leftness;}
  in
  (* TODO we should probably filter the coordinate set to get rid of anything
   * that rests outside the new substrate if rightness or upness is negative *)
  let rightify glyph =
    { glyph with width = glyph.width + rightness;}
  in
  let raise glyph =
    { glyph with height = glyph.height + upness; }
  in
  leftify glyph |> rightify |> raise

let real_glyph font_metadata debug =
  (many1 (glyph_label debug)) >>= fun labels ->
  if debug then Format.eprintf "got labels %a\n%!" Fmt.(list int) (List.map Uchar.to_int labels);
  bitmap debug >>= fun glyph ->
  many metadata >>= fun glyph_metadata ->
  if debug then Format.eprintf "got a %d by %d glyph with %d properties!\n%!" glyph.width glyph.height (List.length glyph_metadata);
  if debug then Format.eprintf "properties are %a\n%!" Fmt.(list ~sep:cut (pair ~sep:comma string string)) glyph_metadata;
  (* some yaff files include the random garbage that sat in here from
   * the original charset, which we probably don't want to do.
   * "fix" this by discarding everything obviously wrong. *)
  match List.filter (fun label ->

      ((Uchar.to_int label) >= 0x20 && (Uchar.to_int label) < 127)
      || Uchar.to_int label > 255)
      labels with
  | [] -> return None
  | labels ->
    let glyph = normalize ~glyph_metadata ~font_metadata glyph in
    return (Some (glyph, labels))

(* some yaffs define a glyph for "missing" characters; ignore it *)
let missing_glyph debug =
  missing >>= fun _ ->
  bitmap debug >>= fun _ ->
  many metadata >>= fun _ ->
  return None

 let glyph font_metadata debug =
  peek_string 10 >>= fun s ->
  if debug then Format.eprintf "looking for a glyph starting at %S\n%!" s;
  (real_glyph font_metadata debug <|> missing_glyph debug)

let placeholder debug =
  (* for some reason, it seems common to put a one-dash placeholder
   * for some characters? I'm not sure what the point is *)
  if debug then Format.eprintf "placeholder?\n%!";
  take_while (Char.equal ' ') >>= fun _ ->
  char '-' >>= fun _ -> return None

let glyph_or_placeholder font_metadata debug =
  placeholder debug <|> glyph font_metadata debug

let comment =
  char '#' >>= fun _ ->
  take_till (Char.equal '\n') >>= fun _ ->
  end_of_line

let glyphs font_metadata debug =
  (sep_by1 (many end_of_line >>= fun _ -> return ())
    (many comment >>= fun _ -> glyph_or_placeholder font_metadata debug)) >>| List.filter_map (fun a -> a)

let yaff debug =
  many metadata >>= fun m ->
  let () =
    if debug then begin
      Format.eprintf "metadata parse concluded: %a\n%!" Fmt.(list ~sep:cut @@ pair ~sep:comma string string) m;
      Format.eprintf "trying for glyphs next\n%!";
    end else ()
  in
  many end_of_line >>= fun _ ->
  option None (glyphs m debug >>| Option.some)

let glyphmap_of_buffer debug cs : (glyphmap, error) result =
  let open Rresult in
  let bs = Cstruct.to_bigarray cs in
  Angstrom.parse_bigstring ~consume:Prefix (yaff debug) bs >>= function
  | Some glyphmap -> Ok glyphmap
  | None -> Error "general parse failure"
