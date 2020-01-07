open Stitchy.Types

let make_substrate background grid phrase interline =
  let {Chars.height; width; _ } = Chars.get_dimensions phrase interline in
  { background;
    grid;
    max_x = max 0 (width - 1);
    max_y = max 0 (height - 1);
  }

let blocks_of_phrase (lookup : Uchar.t -> Stitchy.Types.glyph option) block phrase interline =
  let add_blocks_for_glyph ~x_off ~y_off letter blockmap =
    match lookup letter with
    | None -> blockmap
    | Some glyph -> List.fold_left (fun m (x, y) ->
        BlockMap.add (x_off + x, y_off + y) block m
      ) blockmap glyph.Stitchy.Types.stitches
  in
  let rec advance decoder x_off y_off map =
    match Uutf.decode decoder with
    | `End -> map
    | `Malformed _ | `Await -> (* Await is nonsensical since we already have the whole string
                                  [decode] guarantees that "repeated invocation always eventually
                                  returns [`End], even in case of errors", so let's just do that *)
      advance decoder x_off y_off map
    | `Uchar uchar ->
      match Uucp.Gc.general_category uchar with
      | `Zl | `Cc when Uchar.to_char uchar = '\n' -> advance decoder 0 (y_off + Chars.h + interline) map
      | `Zs -> advance decoder (x_off + Chars.w) y_off map
      | `Ll | `Lm | `Lo | `Lt | `Lu
      (* for the moment, we ignore all combining marks *)
      | `Nd | `Nl | `No
      | `Pc | `Pd | `Pe | `Pf | `Pi | `Po | `Ps
      | `Sc | `Sk | `Sm | `So ->
        let map = add_blocks_for_glyph ~x_off ~y_off uchar map in
        advance decoder (x_off + Chars.w) y_off map
      | _ -> (* not a lot of chance we know what to do with this; ignore it *)
        advance decoder x_off y_off map
  in
  let decoder = Uutf.(decoder (`String phrase)) in
  advance decoder 0 0 BlockMap.empty

let normalize phrase =
  Uunf_string.normalize_utf_8 `NFC phrase

let stitch lookup textcolor background gridsize (phrase : string) interline =
  let phrase = normalize phrase in
  let thread = Colors.thread_of_color textcolor in
  let block : block = { thread; stitch = Full; } in
  let substrate = make_substrate background gridsize phrase interline in
  let phrase = blocks_of_phrase lookup block phrase interline in
  {stitches = phrase; substrate;}
