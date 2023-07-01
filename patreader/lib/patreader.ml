open Angstrom

type fabric = {
  version : int;
  width : int;
  height : int;
  cloth_count_width : int;
  cloth_count_height : int;
  fabric_color : int * int * int * int;
}

type metadata = {
  author : string option;
  copyright : string option;
  title : string option;
  fabric : string option;
  instructions : string option;
  keywords : string option;
  website : string option;
}

type palette = Palette.t list

(* TODO I don't see any reason this shouldn't just be a stitch type. *)
type knot = {
  x : int;
  y : int;
  color_index : int;
}

type backstitch = {
  start_x : int;
  start_y : int;
  start_position : int;
  end_x : int;
  end_y : int;
  end_position : int;
  color_index : int;
}

let pp_dechex fmt n =
  Format.fprintf fmt "%d (0x%x)" n n

let pp_rgba fmt (r, g, b, a) =
  Format.fprintf fmt "R%d G%d B%d A%d (#%02x%02x%02x%02x)" r g b a r g b a

let pp_palette = Fmt.list Palette.pp

let pp_fabric fmt fabric =
  Format.fprintf fmt "version %d; pattern size %a by %a; cloth count %a by %a (per inch); fabric color %a"
    fabric.version
    pp_dechex fabric.width
    pp_dechex fabric.height
    pp_dechex fabric.cloth_count_width
    pp_dechex fabric.cloth_count_height
    pp_rgba fabric.fabric_color

let pp_metadata fmt metadata =
  Format.fprintf fmt "author: %a\n" Fmt.(option string) metadata.author;
  Format.fprintf fmt "copyright: %a\n" Fmt.(option string) metadata.copyright;
  Format.fprintf fmt "title: %a\n" Fmt.(option string) metadata.title;
  Format.fprintf fmt "fabric: %a\n" Fmt.(option string) metadata.fabric;
  Format.fprintf fmt "instructions: %a\n" Fmt.(option string) metadata.instructions;
  Format.fprintf fmt "keywords: %a\n" Fmt.(option string) metadata.keywords

let pp_backstitch fmt backstitch =
  Format.fprintf fmt "(%d, %d):%d to (%d, %d):%d , color index %a"
    backstitch.start_x backstitch.start_y backstitch.start_position
    backstitch.end_x backstitch.end_y backstitch.end_position
    pp_dechex backstitch.color_index

let magic = string "PCStitch "
let version = any_char
let more_magic = string " Pattern File"
(* luckily, the versions are in hexadecimal, so we're safe until pcstitch 16 *)
let skip_spaces = advance (0x100 - String.length("PCStitch 7 Pattern File"))

let header =
  (magic *> version) <* more_magic
   <?>
   "file header incorrect -- is this a .pat file?"

let rgba =
  any_uint8 >>= fun r ->
  any_uint8 >>= fun g ->
  any_uint8 >>= fun b ->
  any_uint8 >>= fun a ->
  return (r, g, b, a)

let len_and_str_16 =
  LE.any_uint16 >>= fun length ->
  take length

let len_and_str_32 =
  LE.any_int32 >>= fun length ->
  take (Int32.to_int length)

module V7 = struct
  let width = LE.any_uint16
  and height = LE.any_uint16
  and cloth_count_height = LE.any_uint16
  and cloth_count_width = LE.any_uint16

  let known_postheader =
    skip_spaces *> (* advance to position 0x100 *)
    LE.any_int32 >>= fun _unknown ->
    LE.any_int32 >>= fun _unknown ->
    LE.any_uint16 >>= fun width ->
    LE.any_uint16 >>= fun height ->
    LE.any_uint16 >>= fun cloth_count_width -> (* e.g., 14x14 for 14-count Aida *)
    LE.any_uint16 >>= fun cloth_count_height ->
    rgba >>= fun fabric_color ->
    return {version = 7;
            width; height; cloth_count_width; cloth_count_height; fabric_color}

  let metadata =
    (* call this at the beginning of the variable length metadata stuff, which is at 0x114 .
     * that's after known_postheader *)
    len_and_str_16 >>= fun author ->
    len_and_str_16 >>= fun copyright ->
    len_and_str_16 >>= fun title ->
    len_and_str_16 >>= fun fabric ->
    len_and_str_16 >>= fun instructions ->
    len_and_str_16 >>= fun keywords ->
    len_and_str_16 >>= fun website ->
    let maybe s = if String.length s > 0 then Some s else None in
    return { author = maybe author;
             copyright = maybe copyright;
             title = maybe title;
             fabric = maybe fabric;
             instructions = maybe instructions;
             keywords = maybe keywords;
             website = maybe website;
    }

  let palette =
    (* we expect this to start right after the end of the "website" metadata. *)
    advance 4 *>
    string "PCStitch Floss Palette!!!" *>
    advance 4 *>
    LE.any_uint16 >>= fun num_palette_entries ->
    count num_palette_entries Palette.unversioned_entry

  let stitch =
    LE.any_uint16 >>= fun n_stitches ->
    any_uint8 >>= fun stitch_color_index -> (* nb this is 1-indexed :eyeroll: *)
    any_uint8 >>= fun stitch_type ->
    return (n_stitches, stitch_color_index, stitch_type)

  let stitches ~width ~height =
    let stitch_coordinates base_index ~new_color ~new_type n =
      let y = (base_index + n) mod height
      and x = (base_index + n) / height
      in
      ((x, y), new_color, new_type)
    in
    let rec aux expected (so_far, stitches) =
      if so_far >= expected
      then return stitches
      else begin
        stitch >>= fun (n_stitches, new_color, new_type) ->
        let new_stitches = List.init n_stitches (stitch_coordinates so_far ~new_color ~new_type) in
        aux expected (so_far + n_stitches, stitches @ new_stitches)
      end
    in
    aux (width * height) (0, [])

  let extra =
    (* I don't know what an "extra" is, nor how to render it. *)
    LE.any_uint16 >>= fun _x_coordinate ->
    LE.any_uint16 >>= fun _y_coordinate ->
    let detailed_info = 
      any_uint8 >>= fun _color_index ->
      any_uint8 >>= fun _stitch_type ->
      return ()
    in
    count 4 detailed_info

  let extras =
    LE.any_int32 >>= fun num_extras ->
    count (Int32.to_int num_extras) extra

  let knot =
    (* I *do* know what a knot is, although we don't have a way to render that atm. *)
    LE.any_uint16 >>= fun x ->
    LE.any_uint16 >>= fun y ->
    LE.any_uint16 >>= fun color_index ->
    return {x; y; color_index }

  let knots =
    LE.any_int32 >>= fun num_knots ->
    count (Int32.to_int num_knots) knot
    
  let backstitch =
    LE.any_uint16 >>= fun start_x ->
    LE.any_uint16 >>= fun start_y ->
    LE.any_uint16 >>= fun start_position -> (* "1..9 based on snap points" *)
    LE.any_uint16 >>= fun end_x ->
    LE.any_uint16 >>= fun end_y ->
    LE.any_uint16 >>= fun end_position -> (* "1..9 based on snap points" *)
    LE.any_uint16 >>= fun color_index ->
    return {start_x; start_y; start_position; end_x; end_y; end_position; color_index}

  let backstitches =
    LE.any_int32 >>= fun num_backstitches ->
    count (Int32.to_int num_backstitches) backstitch
end

module V8 = struct
  let id =
    LE.any_int32 >>= fun num5 ->
    LE.any_int32 >>= fun num6 ->
    LE.any_int32 >>= fun num7 ->
    len_and_str_32 >>= fun text ->
    return (num5, num6, num7, text)

  let argb =
    any_uint8 >>= fun alpha ->
    any_uint8 >>= fun red ->
    any_uint8 >>= fun green->
    any_uint8 >>= fun blue ->
    return (alpha, red, green, blue)

  let unit_of_measure =
    LE.any_int32 >>| function
    | 1l -> "Inches"
    | 2l -> "Centimeters"
    | _  -> "unknown"

  let underlay =
    LE.any_uint16 >>= function
    | 0 -> return ()
    | n ->
      advance n >>= fun () ->
      LE.any_int32 >>= fun _xpos ->
      LE.any_int32 >>= fun _ypos ->
      LE.any_int32 >>= fun _width ->
      LE.any_int32 >>= fun _height ->
      LE.any_int32 >>= fun _xnudge ->
      LE.any_int32 >>= fun _ynudge ->
      return ()

  let metadata fabric =
    len_and_str_32 >>= fun instructions ->
    len_and_str_32 >>= fun keywords ->
    len_and_str_32 >>= fun website ->
    len_and_str_32 >>= fun copyright ->
    len_and_str_32 >>= fun author ->
    len_and_str_32 >>= fun title ->
    len_and_str_32 >>= fun _company ->
    let maybe s = if String.length s > 0 then Some s else None in
    return { author = maybe author;
             copyright = maybe copyright;
             title = maybe title;
             fabric = maybe fabric;
             instructions = maybe instructions;
             keywords = maybe keywords;
             website = maybe website;
    }

  let postheader =
    LE.any_int32 >>= fun _crc ->
    (* TODO: this may be a meaningful field for us --
     * 1 and 2 have a certain behavior, anything else another *)
    len_and_str_32 >>= fun _owner ->
    LE.any_int32 >>= fun _first_num -> (* 'num' *)
    LE.any_uint16 >>= fun _read_only ->
    LE.any_uint16 >>= fun _flag -> (* 'flag' *)
    LE.any_uint16 >>= fun _allow_mapping ->
    LE.any_int32 >>= fun number_of_ids -> (* this value, minus 1, is the upper bound on a loop that reads 3 numbers and does some stuff *)
    count (Int32.to_int number_of_ids) id >>= fun _ids ->
    LE.any_int32 >>= fun save_version ->
    (* for save versions >= 80200, we need to read another number, which we never use *)
    (if save_version >= 80200l then advance 4 else advance 0) >>= fun () ->
    LE.any_int32 >>= fun quickload_height ->
    LE.any_int32 >>= fun quickload_width ->
    match Int32.unsigned_to_int quickload_width,
          Int32.unsigned_to_int quickload_height with
    | Some w, Some h -> return (w, h)
    | None, Some h -> return (0, h)
    | Some w, None -> return (w, 0)
    | None, None -> return (0, 0)

  let substrate =
    LE.any_int32 >>= fun cloth_count_x ->
    LE.any_int32 >>= fun cloth_count_y ->
    len_and_str_32 >>= fun fabric ->
    return (cloth_count_x, cloth_count_y, fabric)

  let postmetadata =
    len_and_str_32 >>= fun default_note_font ->
    Format.eprintf "default note font: %S\n%!" default_note_font;
    argb >>= fun _note_color ->
    unit_of_measure >>= fun _units ->
    len_and_str_32 >>= fun _icon ->
    Format.eprintf "icon read\n%!";
    (* this looks like it should be a char *)
    LE.any_uint16 >>= fun _display_rulers ->
    len_and_str_32 >>= fun _background_bitmap ->
    Format.eprintf "background bitmap read\n%!";
    argb >>= fun background_color -> (* hey, we actually care about this! *)
    (* tons of display logic stuff *)
    LE.any_int32 >>= fun _zoom_level ->
    LE.any_int32 >>= fun _stitch_display_enum ->
    LE.any_uint16 >>= fun _display_grid_lines ->
    LE.any_int32 >>= fun _normal_gridline_width ->
    argb >>= fun _normal_gridline_color ->
    LE.any_int32 >>= fun _bold_gridline_width ->
    argb >>= fun _bold_gridline_color ->
    LE.any_int32 >>= fun _x_bold_lines_at ->
    LE.any_int32 >>= fun _y_bold_lines_at ->
    len_and_str_32 >>= fun _thumbnail ->
    Format.eprintf "thumbnail read\n%!";
    len_and_str_32 >>= fun _logo ->
    Format.eprintf "logo read\n%!";
    underlay >>= fun _underlay ->
    Format.eprintf "underlay read\n%!";
    return background_color

  let palette =
    let preamble = "PCStitch " in
    advance (String.length preamble) >>= fun () ->
    take 2 >>= fun version ->
    advance (25 - 2 - (String.length preamble)) >>= fun () ->
    (* there are unfortunately a few options for the palette format, and they appear
     * to be decoupled from the overall file format (!!).
     * *)
    (* and doubly unfortunately, PCStitch has two options: PCStitch 8 and PCStitch 8a. *)
    match version with
    | "8a" -> Palette.v8a
    | _ -> fail "unknown palette version"

  let postpalette =
    let note =
      len_and_str_32 >>= fun _text ->
      len_and_str_32 >>= fun _rtftext ->
      len_and_str_32 >>= fun _title ->
      LE.any_uint16 >>= fun _x ->
      LE.any_uint16 >>= fun _y ->
      LE.any_uint16 >>= fun _grid_point ->
      LE.any_int32 >>= fun _index ->
      len_and_str_32 >>= fun _icon ->
      len_and_str_32 >>= fun _bitmap ->
      return ()
    in
    let instruction_note =
      (* TODO. We don't have any currently. *)
      return ()
    in
    LE.any_int32 >>= fun num_notes ->
    Format.eprintf "%ld notes\n%!" num_notes;
    count (Int32.to_int num_notes) note >>= fun _ ->
    LE.any_int32 >>= fun num_instruction_notes ->
    Format.eprintf "%ld instruction notes\n%!" num_instruction_notes;
    count (Int32.to_int num_instruction_notes) instruction_note >>= fun _ ->
    return ()

  let stitches =
    let stitch =
      LE.any_uint16 >>= fun x ->
      LE.any_uint16 >>= fun y ->
      LE.any_uint16 >>= fun _grid_point ->
      LE.any_int32 >>= fun palette_index ->
      LE.any_int32 >>= fun stitch_type ->
      if stitch_type < 0l || stitch_type > 1l then Format.eprintf "unusual stitch type: %ld\n%!" stitch_type;
      return ((x, y),
              (Int32.to_int palette_index),
              (Int32.to_int stitch_type))
    in
    let n_below ((x, y), palette_index, stitch_type) n =
      ((x, y+n), palette_index, stitch_type)
    in
    LE.any_int32 >>= fun _num ->
    LE.any_int32 >>= fun _num2 ->
    LE.any_int32 >>= fun width ->
    LE.any_int32 >>= fun height ->
    Format.eprintf "stitch matrix is %ld by %ld\n%!" width height;
    (* TODO: there might be "extras" implied by the number of
     * stitches we see vs the width and height here *)
    LE.any_int32 >>= fun _ccx ->
    LE.any_int32 >>= fun _ccy ->
    LE.any_int32 >>= fun _zorder ->
    (* the structure here seems to be a do/while loop *)
    (* it seems like we have the format: n blanks, start reading stitches;
     * n stitches, stop reading stitches
     * where num_3 is both blanks and stitches?
     * christ *)
    (* hilariously the order is column-major? since we get the x and y coordinates
     * in the points it shouldn't really matter, but lol *)
    let total_to_read = Int32.mul width height in
    let rec keep_reading_stitches read_so_far acc =
      if read_so_far >= total_to_read then return acc else begin
        LE.any_int32 >>= fun num_thing ->
        LE.any_int32 >>= fun stitch_or_blank -> (* 1 for stitch, -1 for blank *)
        let x = Int32.rem read_so_far height
        and y = Int32.div read_so_far width
        in
        Format.eprintf "%ld somethings, type %ld, starting at %ld (%ld, %ld)\n%!" num_thing stitch_or_blank read_so_far x y;
        if stitch_or_blank >= 0l then begin
          stitch >>= fun stitch ->
          (* starting at `to_go`, and increasing `y` until `num_blank` is exhausted (or the end
           * of a column, whichever comes first), we have such a stitch *)
          let new_stitches = List.init (Int32.to_int num_thing) (n_below stitch) in
          keep_reading_stitches (Int32.add read_so_far num_thing) (acc @ new_stitches)
        end else
          keep_reading_stitches (Int32.add read_so_far num_thing) acc
      end
    in
    keep_reading_stitches 0l []
    (* after this, there are also lines, french knots, beads. I don't care about those right now *)
end

let file =
  header >>= function
  | '7' ->
    V7.known_postheader >>= fun fabric ->
    V7.metadata >>= fun metadata ->
    V7.palette >>= fun palette ->
    V7.stitches ~width:fabric.width ~height:fabric.height >>= fun stitches ->
    V7.extras >>= fun extras ->
    V7.knots >>= fun knots ->
    V7.backstitches >>= fun backstitches ->
    return (fabric, metadata, palette, stitches, extras, knots, backstitches)
  | '8' ->
    V8.postheader >>= fun (width, height) ->
    Format.eprintf "width %d, height %d\n%!" width height;
    V8.substrate >>= fun (cloth_count_width, cloth_count_height, fabric) ->
    let cloth_count_width = Int32.to_int cloth_count_width
    and cloth_count_height = Int32.to_int cloth_count_height
    in
    Format.eprintf "ccw %d, cch %d\n%!"
      cloth_count_width cloth_count_height;
    V8.metadata fabric >>= fun metadata ->
    Format.eprintf "metadata: %a\n%!" pp_metadata metadata;
    V8.postmetadata >>= fun bgcolor ->
    Format.eprintf "background color: %a\n%!" pp_rgba bgcolor;
    V8.palette >>= fun palette ->
    Format.eprintf "palette: %a\n%!" (Fmt.list Palette.pp) palette;
    V8.postpalette >>= fun _ ->
    let fabric : fabric = {
      version = 8;
      width; height;
      cloth_count_width; cloth_count_height;
      fabric_color = bgcolor;
    } in
    V8.stitches >>= fun stitches ->
    return (fabric, metadata, palette, stitches, [], [], [])
  | _ -> fail "unknown file format -- try a version 7 or 8 file"
