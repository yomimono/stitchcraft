open Angstrom

type t = {
  scheme : string;
  color_name : string;
  color_description : string;
  color : int * int * int * int;
  symbol_font : string;
  stitch_bytes : int list; (* TODO this should be an enum if it's actually useful. *)
}

let pp_rgba fmt (r, g, b, a) =
  Format.fprintf fmt "R%d G%d B%d A%d (#%02x%02x%02x%02x)" r g b a r g b a

let pp fmt e =
  Format.fprintf fmt "scheme %s, color name %s, description %s, color %a"
    e.scheme e.color_name e.color_description pp_rgba e.color

let rgba =
  any_uint8 >>= fun r ->
  any_uint8 >>= fun g ->
  any_uint8 >>= fun b ->
  any_uint8 >>= fun a ->
  return (r, g, b, a)

let len_and_str_32 =
  LE.any_int32 >>= fun length ->
  take (Int32.to_int length)

let unversioned_entry =
  take 33 >>= fun scheme ->
  take 10 >>= fun color_name ->
  take 30 >>= fun color_description ->
  take 4 >>= fun _unknown1 ->
  rgba >>= fun color ->
  take 81 >>= fun _unknown2 ->
  take 30 >>= fun symbol_font ->
  take 7 >>= fun _unknown3 ->
  take 30 >>= fun _repeated_color_description ->
  rgba >>= fun _repeated_rgba ->
  take 10 >>= fun _repeated_color_name ->
  count 7 any_uint8 >>= fun stitch_bytes ->
  return {scheme; color_name; color_description; color; symbol_font; stitch_bytes }

let v8a =
  let floss file_version =
    len_and_str_32 >>= fun manufacturer_key ->
    len_and_str_32 >>= fun manufacturer_id ->
    len_and_str_32 >>= fun _manufacturer ->
    LE.any_int32 >>= fun _entry_type -> (* again?! *)
    len_and_str_32 >>= fun desc -> (* this again too?! *)
    LE.any_uint16 >>= fun red -> (* sic. this is a 16-bit field *)
    LE.any_uint16 >>= fun green ->
    LE.any_uint16 >>= fun blue ->
    (* then we get an ARGB read (bytes), for some reason *)
    (* I'll admit I don't really know what to do with that *)
    advance 4 >>= fun () ->
    LE.any_uint16 >>= fun _idsort ->
    LE.any_uint16 >>= fun _color_card_column ->
    LE.any_uint16 >>= fun _color_card_row ->
    len_and_str_32 >>= fun default_symbol ->
    Format.eprintf "default symbol: %S\n%!" default_symbol;
    (if file_version >= 4l then begin
        advance 4 >>= fun _second_color ->
        LE.any_int32 >>= fun _is_variegated ->
        return ()
      end else return ()) >>= fun () ->
    return {scheme = manufacturer_key;
            color_name = manufacturer_id;
            color_description = desc;
            (* just throw out the alpha channel, it's always gonna be 255 *)
            color = (red, green, blue, 255);
            (* I don't think I care about these even though I'm passing them around *)
            symbol_font = "";
            stitch_bytes = [];
           }
  in
  let symbol =
    advance 0 >>= fun () ->
    Format.eprintf " <--- entering symbol processing --->\n%!";
    len_and_str_32 >>= fun font_name ->
    Format.eprintf "font name: %S\n%!" font_name;
    LE.any_int32 >>= fun symbol_index ->
    Format.eprintf "symbol index: %ld\n%!" symbol_index;
    LE.any_int32 >>= fun zoom_level ->
    Format.eprintf "zoom level: %lx\n%!" zoom_level;
    LE.any_int32 >>= fun _fg_flag ->
    take 4 >>= fun _fg_color ->
    LE.any_int32 >>= fun _bg_flag ->
    take 4 >>= fun _bg_color ->
    LE.any_int32 >>= fun _italic ->
    LE.any_int32 >>= fun _bold ->
    LE.any_int32 >>= fun _style ->
    LE.any_int32 >>= fun _font_list_index ->
    LE.any_uint16 >>= fun _flag ->
    LE.any_int32 >>= fun stitch_style ->
    Format.eprintf "stitch style: %lx\n%!" stitch_style;
    Format.eprintf " <--- exiting symbol processing --->\n%!";
    return ()
  in
  let palette_entry file_version =
    (* we do a lot of work to unearth the floss,
     * and we want to just ditch everything else *)
    let maybe_variegate = function
      | 0 -> return None
      | _ -> floss file_version >>| fun _ -> None
    in
    take 4 >>= fun entry_type ->
    Printf.eprintf "entry type: %S\n%!" entry_type;
    len_and_str_32 >>= fun id ->
    Printf.eprintf "palette entry id: %S\n%!" id;
    len_and_str_32 >>= fun descr ->
    Format.eprintf "palette entry descr: %S\n%!" descr;
    rgba >>= fun _border_color ->
    LE.any_uint16 >>= fun flag ->
    floss file_version >>= fun main_floss ->
    Format.eprintf "got the main floss: %a \n%!" pp main_floss;
    Printf.eprintf "should we variegate? %d\n%!" flag;
    maybe_variegate flag >>= fun _blend_floss ->
    LE.any_int32 >>= fun line_width ->
    LE.any_int32 >>= fun line_style ->
    LE.any_int32 >>= fun knot_width ->
    LE.any_int32 >>= fun knot_style ->
    LE.any_int32 >>= fun hatch_style ->
    Printf.eprintf "stuff I don't care about: %ld %ld %ld %ld %ld\n%!" line_width line_style knot_width knot_style hatch_style;
    LE.any_int16 >>= fun num_symbols ->
    let real_num_symbols = num_symbols + 1 in
    Printf.eprintf "gonna get %d symbols\n%!" real_num_symbols;
    count (real_num_symbols) symbol >>= fun _symbols ->
    LE.any_int32 >>= fun _index ->
    (* only need this for version >= 1 *)
    (* strand counts for various stuff *)
    (if file_version >= 1l then advance (9 * 4) else return ()) >>= fun () ->
    (* user sort is here in version >= 3 *)
    (if file_version >= 3l then advance 4 else return ()) >>= fun () ->
    Printf.eprintf "ok, done with this palette entry\n%!";
    return main_floss
  in
  let font =
    len_and_str_32 >>= fun _text ->
    LE.any_int32 >>= fun len ->
    advance (Int32.to_int len)
  in
  let maybe_read_fonts = function
    | 0 -> return ()
    | _ ->
      LE.any_int32 >>= fun num_fonts ->
      count (Int32.to_int num_fonts) font >>= fun _ -> return ()
  in
  LE.any_int32 >>= fun save_version ->
  Printf.eprintf "palette save version: %ld\n%!" save_version;
  (* for save versions >= 80200, we need to read another number, which we never use *)
  (if save_version >= 80200l then LE.any_int32 else return 0l) >>= fun file_version ->
  LE.any_int32 >>= fun _default_line_width ->
  LE.any_int32 >>= fun _default_french_knot_width ->
  LE.any_int32 >>= fun _default_bead_width ->
  LE.any_int32 >>= fun _default_bead_height ->
  LE.any_int32 >>= fun _full_stitch ->
  LE.any_int32 >>= fun _three_quarter_stitch ->
  LE.any_int32 >>= fun _quarter_stitch ->
  LE.any_int32 >>= fun _petite ->
  LE.any_int32 >>= fun _half ->
  LE.any_int32 >>= fun _french_knot ->
  LE.any_int32 >>= fun _backstitch ->
  LE.any_int32 >>= fun num_palette_entries ->
  Format.eprintf "%ld palette entries\n%!" num_palette_entries;
  count (Int32.to_int num_palette_entries) (palette_entry file_version) >>= fun palette ->
  Format.eprintf "got 'em\n%!";
  LE.any_uint16 >>= fun embed_fonts ->
  maybe_read_fonts embed_fonts >>= fun _ ->
  return palette
