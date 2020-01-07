open Stitchy.Types

let make_substrate ~max_x ~max_y background grid =
  { background;
    grid;
    max_x;
    max_y;
  }

(* TODO: this logic aggressively assumes a monospaced font;
   instead, the dimensions of each character should be individually
   discoverable and calculations made on them *)
let blocks_of_phrase (lookup : Uchar.t -> Stitchy.Types.glyph option) block phrase interline =
  let add_blocks_for_glyph ~x_off ~y_off letter blockmap =
    match lookup letter with
    | None -> blockmap
    | Some glyph -> List.fold_left (fun m (x, y) ->
        BlockMap.add (x_off + x, y_off + y) block m
      ) blockmap glyph.Stitchy.Types.stitches
  in
  let rec advance decoder x_off y_off (map, max_x, max_y) =
    let new_max_x = max (x_off + Chars.w) max_x in (* only if this is a space-occupying non-newline *)
    match Uutf.decode decoder with
    | `End -> (map, max_x, max_y)
    | `Malformed _ | `Await -> (* Await is nonsensical since we already have the whole string
                                  [decode] guarantees that "repeated invocation always eventually
                                  returns [`End], even in case of errors", so let's just do that *)
      advance decoder x_off y_off (map, max_x, max_y)
    | `Uchar uchar ->
      match Uucp.Gc.general_category uchar with
      | `Zl | `Cc when Uchar.to_char uchar = '\n' ->
        let y_increase = Chars.h + interline in
        advance decoder 0 (y_off + y_increase) (map, max_x, max_y + y_increase)
      | `Zs -> advance decoder (x_off + Chars.w) y_off (map, new_max_x, max_y)
      | `Ll | `Lm | `Lo | `Lt | `Lu
      (* for the moment, we ignore all combining marks *)
      | `Nd | `Nl | `No
      | `Pc | `Pd | `Pe | `Pf | `Pi | `Po | `Ps
      | `Sc | `Sk | `Sm | `So ->
        let map = add_blocks_for_glyph ~x_off ~y_off uchar map in
        advance decoder (x_off + Chars.w) y_off (map, new_max_x, max_y)
      | _ -> (* not a lot of chance we know what to do with this; ignore it *)
        advance decoder x_off y_off (map, max_x, max_y)
  in
  let decoder = Uutf.(decoder (`String phrase)) in
  advance decoder 0 0 (BlockMap.empty, Chars.w, Chars.h)

let normalize phrase =
  Uunf_string.normalize_utf_8 `NFC phrase

let stitch lookup textcolor background gridsize (phrase : string) interline =
  let phrase = normalize phrase in
  let thread = Colors.thread_of_color textcolor in
  let block : block = { thread; stitch = Full; } in
  let (phrase, max_x, max_y) = blocks_of_phrase lookup block phrase interline in
  let substrate = make_substrate ~max_x ~max_y background gridsize in
  {stitches = phrase; substrate;}
