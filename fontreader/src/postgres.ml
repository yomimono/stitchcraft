open Lwt.Infix

type db = { host : string;
            port : int;
            database : string;
            user : string;
            password : string;
          }

let make_glyphs_table connection =
  (* TODO: we definitely want uniqueness here; writes are way more rare than reads *)
  Pgx_lwt_unix.execute_unit connection {|
    CREATE TABLE IF NOT EXISTS glyphs (
        id              bigserial PRIMARY KEY,
        height          integer NOT NULL,
        width           integer NOT NULL,
        stitches        json NOT NULL,
        CONSTRAINT positive CHECK (height >= 0 AND width >= 0)
        )
    |}

let make_fonts_table connection =
  Pgx_lwt_unix.execute_unit connection {|
    CREATE TABLE IF NOT EXISTS fonts (
        id      bigserial PRIMARY KEY,
        name    text NOT NULL,
        CONSTRAINT unique_name UNIQUE(name)
        )
    |}

let make_fonts_glyphs_table connection =
  Pgx_lwt_unix.execute_unit connection {|
    CREATE TABLE IF NOT EXISTS fonts_glyphs (
        font integer references fonts(id),
        uchar integer NOT NULL,
        glyph integer references glyphs(id),
        CONSTRAINT positive CHECK (uchar >= 0)
        )
    |}

let insert debug connection font glyphmap =
  let glyph_cte = {|
  WITH selected_font AS (
    SELECT id FROM fonts WHERE name = $4 LIMIT 1
  ),
  inserted_font AS (
    INSERT INTO fonts (name) VALUES ($4)
    ON CONFLICT DO NOTHING
    RETURNING id
  ),
  glyph AS (
    INSERT INTO glyphs (width, height, stitches) VALUES ($1, $2, $3) RETURNING id
  )
  INSERT INTO fonts_glyphs (font, glyph, uchar)
    SELECT selected_font.id, glyph.id, $5::integer FROM selected_font, glyph
    UNION
    SELECT inserted_font.id, glyph.id, $5::integer FROM inserted_font, glyph
    LIMIT 1
  |}
  in
  (* if we get a (glyph * uchar list) list,
   * we need to generate the list of glyph-uchar pairings for each element,
   * then generate those for each glyph,
   * flatten the list,
   * and use that as params *)
  let params =
    let params_per_uchar Stitchy.Types.({width; height; stitches}) uchar =
      let stitches_json = Stitchy.Types.CoordinateSet.to_yojson stitches |> Yojson.Safe.to_string in
      Pgx.Value.[of_int width;
                 of_int height;
                 of_string stitches_json;
                 of_string font;
                 of_int (Uchar.to_int uchar);]
    in
    let params_per_glyph (glyph, uchars) = List.map (params_per_uchar glyph) uchars in
    if debug then Format.eprintf "inserting %d rows for %d glyphs\n%!"
        List.(length @@ flatten @@ map params_per_glyph glyphmap)
        (List.length glyphmap);
    List.flatten @@ List.map params_per_glyph glyphmap
  in
  if debug then Format.eprintf "about to insert %d rows\n%!" List.(length params);
  Pgx_lwt_unix.execute_many connection ~params ~query:glyph_cte >|= function
  | l ->
    if debug then Format.eprintf "%d rows returned\n%!" List.(length @@ flatten l);
    Ok ()

module Populate(Reader : Fontreader.Readfiles.INTERPRETER) = struct
  let populate {host; port; user; database; password} src font debug =
    match Bos.OS.File.read (Fpath.v src) with
    | Error e -> Error e
    | Ok s ->
      match Reader.glyphmap_of_buffer debug (Cstruct.of_string s) with
      | Error e -> Error (`Msg (Format.asprintf "%a" Reader.pp_error e))
      | Ok (l : (Stitchy.Types.glyph * Uchar.t list) list) ->
        Lwt_main.run (
          Pgx_lwt_unix.connect ~verbose:10 ~host ~port ~user ~database ~password () >>= fun connection ->
          make_glyphs_table connection >>= fun () ->
          make_fonts_table connection >>= fun () ->
          make_fonts_glyphs_table connection >>= fun () ->
          insert debug connection font l
        )
end
