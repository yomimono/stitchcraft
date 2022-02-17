type glyphmap = (Stitchy.Types.glyph * Uchar.t list) list
type error = string

open Angstrom

let pp_error fmt s = Format.fprintf fmt "%s" s

let metadata =
  take_till (Char.equal ':') >>= fun label ->
  char ':' >>= fun _ ->
  take_till (Char.equal '\n') >>= fun value ->
  end_of_line >>= fun _ ->
  if String.length value < 1 then fail "no more"
  else return (label, value)

let eight_bit debug =
  (* these are "plain" 8-bit chars, so we run them through Char.chr *)
  let uchar_of_string s = int_of_string s |> Char.chr |> Uchar.of_char in
  string "0x" >>= fun _ ->
  if debug then Format.eprintf "looks like an eight-bit label\n%!";
  take_till (Char.equal ':') >>| fun s ->
  (* we need to reappend this so int_of_string knows what to do with hex *)
  let hex = "0x" ^ s in
  if debug then Format.eprintf "string value of label: %s\n%!" s;
  uchar_of_string hex

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
    stitches
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

let real_glyph debug =
  (many1 (glyph_label debug)) >>= fun labels ->
  if debug then Format.eprintf "got labels %a\n%!" Fmt.(list int) (List.map Uchar.to_int labels);
  bitmap debug >>| fun glyph ->
  if debug then Format.eprintf "got a %d by %d glyph!\n%!" glyph.width glyph.height;
  (* some yaff files include the random garbage that sat in here from
   * the original charset, which we probably don't want to do.
   * "fix" this by discarding everything obviously wrong. *)
  match List.filter (fun label ->

      ((Uchar.to_int label) >= 0x20 && (Uchar.to_int label) < 127)
      || Uchar.to_int label > 255)
      labels with
  | [] -> None
  | labels -> Some (glyph, labels)

(* some yaffs define a glyph for "missing" characters; ignore it *)
let missing_glyph debug =
  missing >>= fun _ ->
  bitmap debug >>| fun _ ->
  None

let glyph debug =
  peek_string 10 >>= fun s ->
  if debug then Format.eprintf "looking for a glyph starting at %S\n%!" s;
  (real_glyph debug <|> missing_glyph debug)

let placeholder debug =
  (* for some reason, it seems common to put a one-dash placeholder
   * for some characters? I'm not sure what the point is *)
  if debug then Format.eprintf "placeholder?\n%!";
  take_while (Char.equal ' ') >>= fun _ ->
  char '-' >>= fun _ -> return None

let glyph_or_placeholder debug =
  placeholder debug <|> glyph debug

let comment =
  char '#' >>= fun _ ->
  take_till (Char.equal '\n') >>= fun _ ->
  end_of_line

let glyphs debug =
  (sep_by1 (many end_of_line >>= fun _ -> return ())
    (many comment >>= fun _ -> glyph_or_placeholder debug)) >>| List.filter_map (fun a -> a)

let yaff debug =
  many metadata >>= fun m ->
  let () =
    if debug then begin
      Format.eprintf "metadata parse concluded: %a\n%!" Fmt.(list @@ pair string string) m;
      Format.eprintf "trying for glyphs next\n%!";
    end else ()
  in
  many end_of_line >>= fun _ ->
  option None (glyphs debug >>| Option.some)

let glyphmap_of_buffer debug cs : (glyphmap, error) result =
  let open Rresult in
  let bs = Cstruct.to_bigarray cs in
  Angstrom.parse_bigstring ~consume:Prefix (yaff debug) bs >>= function
  | Some glyphmap -> Ok glyphmap
  | None -> Error "general parse failure"
