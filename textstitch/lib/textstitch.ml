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

let add_glyph_to_layers ~x_off ~y_off glyph (layer : layer) (bs_layer : backstitch_layer)
  : (layer * backstitch_layer) =
  let move (x, y) =
    (x + x_off, y + y_off)
  in
  let with_new_stitches =
    CoordinateSet.fold (fun (x, y) stitches ->
        CoordinateSet.add (move (x, y)) stitches
      ) glyph.Stitchy.Types.stitches layer.stitches
  in
  let with_new_backstitches =
    SegmentSet.fold (fun (src, dst) backstitches ->
        SegmentSet.add (move src, move dst) backstitches
      ) glyph.Stitchy.Types.backstitches bs_layer.stitches
  in
  {layer with stitches = with_new_stitches}, {bs_layer with stitches = with_new_backstitches}

let uchars_of_phrase phrase =
  let phrase = Uunf_string.normalize_utf_8 `NFC phrase in
  let rec advance decoder uchars =
    match Uutf.decode decoder with
    | `End -> List.rev uchars
    | `Malformed _ | `Await -> (* Await is nonsensical since we already have the whole string
                                  [decode] guarantees that "repeated invocation always eventually
                                  returns [`End], even in case of errors", so let's just do that *)
      advance decoder uchars
    | `Uchar uchar ->
      match Uucp.Gc.general_category uchar with
      (* newlines *)
      | `Zl | `Cc when Uchar.to_char uchar = '\n' ->
        advance decoder (uchar::uchars)
      (* single-width whitespace *)
      | `Zs -> advance decoder @@ (Uchar.of_char ' ')::uchars
      | `Ll | `Lm | `Lo | `Lt | `Lu
      (* for the moment, we ignore all combining marks *)
      (* there are many fonts for which we could do the right thing here -- TODO *)
      | `Nd | `Nl | `No
      | `Pc | `Pd | `Pe | `Pf | `Pi | `Po | `Ps
      | `Sc | `Sk | `Sm | `So ->
        (* "normal" case; we can do stuff with this uchar *)
        advance decoder @@ uchar::uchars
      | _ -> (* not a lot of chance we know what to do with this; ignore it *)
        advance decoder uchars
  in
  let decoder = Uutf.(decoder (`String phrase)) in
  advance decoder []

let render_phrase (lookup : Uchar.t -> Stitchy.Types.glyph option) thread uchars ~min_height ~min_width interline =
  let add_stitches_for_glyph ~x_off ~y_off letter layer bs_layer =
    match lookup letter with
    | None -> layer, bs_layer
    | Some glyph -> add_glyph_to_layers ~x_off ~y_off glyph layer bs_layer
  in
  let empty_layer = {
    thread;
    stitch = Cross Full;
    stitches = CoordinateSet.empty;
  } in
  let empty_bs_layer = {
    thread;
    stitches = SegmentSet.empty
  } in
  let _, _, stitches, backstitches, max_x, max_y =
    (* TODO: this is going to be wrong sometimes for variable-width fonts.
     * We should probably have a look-uppable or settable value for the
     * default font size *)
    let (starting_x, starting_y) = get_dims lookup default_char in
    let (starting_x, starting_y) = max min_width starting_x, max min_height starting_y in
    List.fold_left (fun (x_off, y_off, stitches, backstitches, max_x, max_y) uchar ->
        match Uucp.Gc.general_category uchar with
        | `Zl | `Cc when Uchar.to_char uchar = '\n' ->
          let _, height = get_dims lookup default_char in
          let height = max min_height height in
          let y_increase = height + interline in
          (0, y_off + y_increase,
           stitches, backstitches, max_x, max_y + y_increase)
        | `Ll | `Lm | `Lo | `Lt | `Lu
        (* for the moment, we ignore all combining marks *)
        (* there are many fonts for which we could do the right thing here -- TODO *)
        | `Nd | `Nl | `No
        | `Pc | `Pd | `Pe | `Pf | `Pi | `Po | `Ps
        | `Sc | `Sk | `Sm | `So
        | `Zs ->
          (* TODO: we should probably center or something when given a min_dimension
           * larger than the one we looked up? *)
          let width, _ = get_dims lookup uchar in
          let width = max min_width width in
          let new_max_x = max (x_off + width) max_x in
          let stitches, backstitches = add_stitches_for_glyph ~x_off ~y_off uchar stitches backstitches in
          ((x_off + width), y_off, stitches, backstitches, new_max_x, max_y)
        | _ -> (* not a lot of chance we know what to do with this; ignore it *)
          (x_off, y_off, stitches, backstitches, max_x, max_y)
      ) (0, 0, empty_layer, empty_bs_layer, starting_x, starting_y) uchars
  in
  (stitches, backstitches, max_x, max_y)

let stitch lookup thread background gridsize uchars ~min_width ~min_height interline =
  let (layer, backstitch_layer, max_x, max_y) = render_phrase lookup thread uchars ~min_width ~min_height interline in
  let substrate = make_substrate ~max_x ~max_y background gridsize in
  {layers = [layer]; substrate; backstitch_layers = [backstitch_layer];}
