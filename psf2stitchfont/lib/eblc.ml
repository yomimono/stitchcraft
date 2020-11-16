(* the "Embedded Bitmap Location Table", as described
   in https://docs.microsoft.com/en-us/typography/opentype/spec/eblc , said document last updated 7th Aug 2018 *)

[%%cstruct
  type header = {
    major_version : uint16_t;
    minor_version : uint16_t;
    num_sizes : uint32_t;
  } [@@big_endian]]

[%%cstruct
  type sbit_line_metrics = {
    ascender : int8_t;
    descender : int8_t;
    width_max : uint8_t;
    caret_slope_numerator : int8_t;
    caret_slope_denominator : int8_t;
    caret_offset : int8_t;
    min_origin_sb : int8_t;
    min_advance_sb : int8_t;
    max_before_bl : int8_t;
    min_after_bl : int8_t;
    pad1 : int8_t;
    pad2 : int8_t;
  } [@@big_endian]]

[%%cstruct
  type bitmap_size = {
    index_sub_table_offset : uint32_t;
    index_tables_size : uint32;
    num_index_sub_tables : uint32;
    (* color_ref is unused according to documentation *)
    color_ref : uint32;
    (* horizontal and vertical are both sbit_line_metrics *)
    horizontal : uint8_t [@len 12];
    vertical : uint8_t [@len 12];
    start_glyph_index : uint16_t;
    end_glyph_index : uint16_t;
    ppem_x : uint8_t;
    ppem_y : uint8_t;
    bit_depth : uint8_t;
    flags : uint8_t;
  } [@@big_endian]]

[%%cstruct
  type index_sub_table_array = {
    first_glyph_index : uint16_t;
    last_glyph_index : uint16_t;
    additional_offset_to_index_table : uint32_t;
  } [@@big_endian]]

[%%cstruct
  type index_sub_header = {
    index_format : uint16_t;
    image_format : uint16_t;
    image_data_offset : uint32_t;
  } [@@big_endian]]

[%%cstruct
  type index_sub_table_2 = {
    image_size : uint32_t;
    big_metrics : uint8 [@len 8];
  } [@@big_endian]]

[%%cstruct
  type big_glyph_metrics = {
    height : uint8_t;
    width : uint8_t;
    horizontal_bearing_x : int8_t;
    horizontal_bearing_y : int8_t;
    horizontal_advance : uint8_t;
    vertical_bearing_x : int8_t;
    vertical_bearing_y : int8_t;
    vertical_advance : uint8_t;
  } [@@big_endian]]

[%%cstruct
  type small_glyph_metrics = {
    height : uint8_t;
    width : uint8_t;
    bearing_x : int8_t;
    bearing_y : int8_t;
    advance : uint8_t;
  } [@@big_endian]]

type size = {
  width : int;
  height : int;
}

type sub_table = {
  offset : int;
  glyph_ids : int list;
  image_format : int;
  metrics : size option; (* if it's a weird format, we might not know *)
  image_size : int option; (* the size of each glyph in... bytes I think? *)
}

let pp_size fmt s = Format.fprintf fmt "width: %d (0x%x), height %d (0x%x)" s.width s.width s.height s.height

let pp_sub_table fmt st = Format.fprintf fmt
    "offset 0x%x, %d (0x%x) glyphs starting with %d (0x%x), image format %d" st.offset (List.length st.glyph_ids) (List.length st.glyph_ids) (List.hd st.glyph_ids) (List.hd st.glyph_ids) st.image_format

let glyph_ids index_entry =
  match get_index_sub_table_array_first_glyph_index index_entry,
        get_index_sub_table_array_last_glyph_index index_entry with
  | q, r when q = r -> [q]
  | a, b when a < b -> List.init (b-a) (fun n -> n + a)
  | b, a -> List.init (b-a) (fun n -> n + a)

let get_subtable_length_from_format sub_table_header =
  let format_number = get_index_sub_header_index_format sub_table_header in
  match format_number with
    | 1 -> 4
    | 2 -> 4 + sizeof_big_glyph_metrics
    | 3 -> 2
    | 4 -> 4 + (* glyph_id_offset_pair *) 2 + 2
    | 5 -> 4 + sizeof_big_glyph_metrics + 4 + 2
    | _ -> 4

let get_index_sub_table_array_entries eblc =
  (* TODO: look, we only have one BitmapSize, OK? *)
  let bitmap_size = Cstruct.shift eblc sizeof_header in
  let num_index_sub_tables = get_bitmap_size_num_index_sub_tables bitmap_size in
  let sub_table_array_offset = get_bitmap_size_index_sub_table_offset bitmap_size in (* this is the start of the sub_table_array *)
  List.init (Int32.to_int num_index_sub_tables) (fun nth_subtable_header ->
      let offset = (Int32.to_int sub_table_array_offset) +
                   (nth_subtable_header *
                    sizeof_index_sub_header) in
      let index_sub_header = Cstruct.shift eblc offset in
      let table_length = sizeof_index_sub_header +
                   get_subtable_length_from_format index_sub_header in
      Cstruct.sub index_sub_header 0 table_length)

let get_sub_tables eblc =
  let bitmap_size = Cstruct.shift eblc sizeof_header in
  let sub_table_array_offset = get_bitmap_size_index_sub_table_offset bitmap_size in
  List.map (fun sub_table_array_entry ->
      let sub_table_location = Int32.(add sub_table_array_offset @@ get_index_sub_header_image_data_offset sub_table_array_entry) in
      let sub_table_location = Int32.to_int sub_table_location in
      let sub_table_header = Cstruct.shift eblc sub_table_location in
      let size_of_table = get_subtable_length_from_format sub_table_header in
      let sub_table = Cstruct.sub eblc sub_table_location (sizeof_index_sub_header + size_of_table) in
      (sub_table_array_entry, sub_table)
    ) (get_index_sub_table_array_entries eblc)

let sub_table_info eblc =
  let info = get_sub_tables eblc in
  List.map (fun (index_entry, sub_table) ->
      let body = Cstruct.shift sub_table sizeof_index_sub_header in
      let glyph_ids = glyph_ids index_entry in
      let offset = get_index_sub_header_image_data_offset sub_table |> Int32.to_int in
      let image_format = get_index_sub_header_image_format sub_table in
      match get_index_sub_header_index_format sub_table with
      | 2 ->
        let image_size = Some (Int32.to_int @@ get_index_sub_table_2_image_size body) in
        let big_glyph_metrics = get_index_sub_table_2_big_metrics body in
        let height = get_big_glyph_metrics_height big_glyph_metrics
        and width = get_big_glyph_metrics_width big_glyph_metrics in
        { offset; glyph_ids; image_format; metrics = Some {width; height}; image_size }
      | _ ->
        (* we can do better here, but I don't care for the moment *)
        { offset; glyph_ids; image_format; metrics = None; image_size = None}
    ) info
