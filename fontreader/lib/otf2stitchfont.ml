[%%cstruct
  type otf_ebdt_header = {
    major_version : uint16_t;
    minor_version : uint16_t;
  } [@@big_endian]]

type error = [`Msg of string | `Format of Otfm.error | `No_glyphmap ]

type glyphmap = Stitchy.Types.glyph list * Uchar.t list list

let pp_error fmt = function
  | `Msg s -> Format.fprintf fmt "%s" s
  | `Format e -> Format.fprintf fmt "%a" Otfm.pp_error e
  | `No_glyphmap -> Format.fprintf fmt "the font does not contain a bitmap glyph table"


let cp_list = function
  | q, r when q = r -> [q]
  | a, b -> List.init (b - a) ((+) a)

let associate_cps (glyph_data : (int * Stitchy.Types.glyph) list) acc map_kind cp_range glyph_id =
  match map_kind with
  | `Glyph -> begin
    match List.assoc_opt glyph_id glyph_data with
    | None -> acc
    | Some data -> (* code points cp_range all map to the associated data *)
      let cps = cp_list cp_range in
      (data, (List.map Uchar.of_int cps)) :: acc
  end
  | `Glyph_range ->
    let cps = cp_list cp_range in
    let l = List.mapi (fun n cp -> (cp, glyph_id + n)) cps in
    let local_list = List.fold_left (fun local_acc (cp, glyph_id) ->
        match List.assoc_opt glyph_id glyph_data with
        | None -> local_acc
        | Some data ->
          (data, [Uchar.of_int cp]) :: local_acc
      ) [] l in
    local_list @ acc


(* we say "map", but it's an associative list of glyphs to lists of uchar.ts *)
(* that feels awkward; why not return them as pairs? I guess because we'd have to reconstruct the snd of the pair all the time and that's a headache. we could zip it before returning I guess *)
let glyphmap_of_buffer debug buffer =
  let source = Otfm.decoder (`String (Cstruct.to_string buffer)) in
  match Otfm.(table_raw source Tag.ebdt, table_raw source Tag.eblc) with
  | Error e, _ | _, Error e -> Error (`Format e)
  | Ok None, _ | _, Ok None -> Error `No_glyphmap
  | Ok (Some ebdt), Ok (Some eblc) ->
    if debug then Printf.printf "decoder found EBDT and EBLC\n%!";
    let ebdt = Cstruct.of_string ebdt in
    let eblc = Cstruct.of_string eblc in
    let sub_tables = Eblc.sub_table_info eblc in
    if debug then Format.printf "sub_tables: %a\n" Fmt.(list Eblc.pp_sub_table) sub_tables;
    let glyph_ids_and_data =
      List.fold_left (fun glyphs_so_far sub_table ->
          if debug then Printf.printf "got %d glyphs so far\n%!" @@ List.length glyphs_so_far;
          match sub_table.Eblc.image_size, sub_table.metrics with
          | None, _ | _, None -> glyphs_so_far
          | Some _, Some metrics ->
            let width = metrics.width and height = metrics.height in
            let offset = sub_table.offset in
            let n_glyphs = List.length sub_table.glyph_ids in
            if debug then Printf.printf "looking to get data for %d glyphs of size %dx%d starting at %x\n%!" n_glyphs width height offset;
            let glyph_data = Ebdt.glyph_data_to_pattern ~offset ~width ~height ebdt n_glyphs in
            glyphs_so_far @ (List.combine sub_table.glyph_ids glyph_data)
        ) [] sub_tables in
    if debug then Printf.printf "got %d glyph ids\n%!" @@ List.length glyph_ids_and_data;
    (* now we need to get the unicode code points or ranges mapped to glyph IDs *)
    match Otfm.cmap source (associate_cps glyph_ids_and_data) [] with
    | Error e -> Error (`Format e)
    | Ok (_, glyphlist) ->
      Ok (List.split glyphlist)
