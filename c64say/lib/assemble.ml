open Stitchy.Types

let make_substrate ~max_x ~max_y background grid =
  { background;
    grid;
    max_x;
    max_y;
  }

let get_dims (lookup : Uchar.t -> Stitchy.Types.glyph option) uchar =
  match lookup uchar with
  | None -> (0, 0)
  | Some g -> (g.width, g.height)

let default_char = Uchar.of_char '#' (* we assume "#" is in most fonts and pretty big *)

let add_glyph_to_layer ~x_off ~y_off glyph (layer : layer) : layer =
  let with_new_stitches =
    CoordinateSet.fold (fun (x, y) stitches ->
        CoordinateSet.add (x_off + x, y_off + y) stitches
      ) glyph.Stitchy.Types.stitches layer.stitches
  in
  {layer with stitches = with_new_stitches}

let render_phrase (lookup : Uchar.t -> Stitchy.Types.glyph option) thread phrase interline =
  let add_stitches_for_glyph ~x_off ~y_off letter layer =
    match lookup letter with
    | None -> layer
    | Some glyph -> add_glyph_to_layer ~x_off ~y_off glyph layer
  in
  let rec advance decoder x_off y_off (stitches, max_x, max_y) =
    match Uutf.decode decoder with
    | `End -> (stitches, max_x, max_y)
    | `Malformed _ | `Await -> (* Await is nonsensical since we already have the whole string
                                  [decode] guarantees that "repeated invocation always eventually
                                  returns [`End], even in case of errors", so let's just do that *)
      advance decoder x_off y_off (stitches, max_x, max_y)
    | `Uchar uchar ->
      match Uucp.Gc.general_category uchar with
      | `Zl | `Cc when Uchar.to_char uchar = '\n' ->
        let _, height = get_dims lookup default_char in
        let y_increase = height + interline in
        advance decoder 0 (y_off + y_increase) (stitches, max_x, max_y + y_increase)
      | `Zs ->
        let width, _ = get_dims lookup default_char in
        let new_max_x = max (x_off + width) max_x in
        advance decoder (x_off + width) y_off (stitches, new_max_x, max_y)
      | `Ll | `Lm | `Lo | `Lt | `Lu
      (* for the moment, we ignore all combining marks *)
      (* there are many fonts for which we could do the right thing here -- TODO *)
      | `Nd | `Nl | `No
      | `Pc | `Pd | `Pe | `Pf | `Pi | `Po | `Ps
      | `Sc | `Sk | `Sm | `So ->
        let width, _ = get_dims lookup uchar in
        let new_max_x = max (x_off + width) max_x in
        let stitches = add_stitches_for_glyph ~x_off ~y_off uchar stitches in
        advance decoder (x_off + width) y_off (stitches, new_max_x, max_y)
      | _ -> (* not a lot of chance we know what to do with this; ignore it *)
        advance decoder x_off y_off (stitches, max_x, max_y)
  in
  let decoder = Uutf.(decoder (`String phrase)) in
  let _, starting_height = get_dims lookup default_char in
  let empty_layer = {
    thread;
    stitch = Cross Full;
    stitches = CoordinateSet.empty;
  } in
  advance decoder 0 0 (empty_layer, 0, starting_height)

let normalize phrase =
  Uunf_string.normalize_utf_8 `NFC phrase

let stitch lookup textcolor background gridsize (phrase : string) interline =
  let phrase = normalize phrase in
  let thread = Colors.thread_of_color textcolor in
  let (phrase, max_x, max_y) = render_phrase lookup thread phrase interline in
  let substrate = make_substrate ~max_x ~max_y background gridsize in
  {layers = [phrase]; substrate;}
