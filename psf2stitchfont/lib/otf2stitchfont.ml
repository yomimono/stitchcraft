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

let associate_cps (glyph_data : (int * Stitchy.Types.glyph) list) acc map_kind cp_range glyph_id =
  match map_kind with
  | `Glyph -> begin
    match List.assoc_opt glyph_id glyph_data with
    | None -> acc
    | Some data -> (* code points cp_range all map to the associated data *)
      let cps = match cp_range with
        | q, r when q = r -> [Uchar.of_int q]
        | a, b -> List.init (b - a) (fun n -> Uchar.of_int @@ n + a)
      in
      (data, cps) :: acc
  end
  | `Glyph_range -> acc (* TODO: don't quite understand this *)


(* we say "map", but it's an associative list of glyphs to lists of uchar.ts *)
(* that feels awkward; why not return them as pairs? I guess because we'd have to reconstruct the snd of the pair all the time and that's a headache. we could zip it before returning I guess *)
let glyphmap_of_buffer buffer =
  let source = Otfm.decoder (`String (Cstruct.to_string buffer)) in
  match Otfm.(table_raw source Tag.ebdt, table_raw source Tag.eblc) with
  | Error e, _ | _, Error e -> Error (`Format e)
  | Ok None, _ | _, Ok None -> Error `No_glyphmap
  | Ok (Some ebdt), Ok (Some eblc) ->
    let ebdt = Cstruct.of_string ebdt in
    let eblc = Cstruct.of_string eblc in
    let sub_tables = Eblc.sub_table_info eblc in
    let glyph_ids_and_data =
      List.fold_left (fun glyphs_so_far sub_table ->
          match sub_table.Eblc.image_size, sub_table.metrics with
          | None, _ | _, None -> glyphs_so_far
          | Some size, Some metrics ->
            let glyphs = List.mapi (fun n glyph_id ->
                let offset = sub_table.offset + n * size in
                let glyph_data = Ebdt.glyph_data_to_pattern ebdt ~width:metrics.width ~height:metrics.height ~size ~offset in
                (glyph_id, glyph_data)
              ) sub_table.glyph_ids
            in
            glyphs_so_far @ glyphs
        ) [] sub_tables in
    Printf.printf "got %d glyph IDs mapped to some data\n%!" @@ List.length glyph_ids_and_data;
    (* now we need to get the unicode code points or ranges mapped to glyph IDs *)
    match Otfm.cmap source (associate_cps glyph_ids_and_data) [] with
    | Error e -> Error (`Format e)
    | Ok (_, glyphlist) -> Ok (`Glyphmap (List.split glyphlist))
