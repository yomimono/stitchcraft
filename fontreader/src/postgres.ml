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
        CONSTRAINT positive CHECK (uchar > 0)
        )
    |}

let insert connection font Stitchy.Types.({width; height; stitches}) uchars =
  let glyph_cte = {|
  WITH glyph AS (
    INSERT INTO glyphs (width, height, stitches) VALUES ($1, $2, $3) RETURNING id
  ), font AS (
    INSERT INTO FONTS (name) VALUES ($4) ON CONFLICT (name) DO NOTHING RETURNING id
  )
  INSERT INTO fonts_glyphs (font, glyph, uchar)
    SELECT font.id, glyph.id, $5
    FROM font, glyph
  |}
  in
  let stitches_json = Stitchy.Types.CoordinateSet.to_yojson stitches |> Yojson.Safe.to_string in
  let params uchar = Pgx.Value.[of_int width;
                          of_int height;
                          of_string stitches_json;
                          of_string font;
                          of_int (Uchar.to_int uchar);]
  in
  Pgx_lwt_unix.execute_many connection ~params:(List.map params uchars) ~query:glyph_cte >|= function
  | l when (List.length l) < (List.length uchars) -> Error (`Msg "not as many rows were inserted as we expected")
  | _ -> Ok ()

module Populate(Reader : Fontreader.Readfiles.INTERPRETER) = struct
  let populate {host; port; user; database; password} src font debug =
    match Bos.OS.File.read (Fpath.v src) with
    | Error e -> Error e
    | Ok s ->
      match Reader.glyphmap_of_buffer debug (Cstruct.of_string s) with
      | Error e -> Error (`Msg (Format.asprintf "%a" Reader.pp_error e))
      | Ok (l : (Stitchy.Types.glyph * Uchar.t list) list) ->
        Lwt_main.run (
          Pgx_lwt_unix.connect ~host ~port ~user ~database ~password () >>= fun connection ->
          make_glyphs_table connection >>= fun () ->
          make_fonts_table connection >>= fun () ->
          make_fonts_glyphs_table connection >>= fun () ->
          Lwt_list.iter_p (fun (glyph, uchars) ->
              insert connection font glyph uchars>>= function
              | Error (`Msg s) ->
                Format.eprintf "Error %s: couldn't insert glyph for %a; skipping those uchars\n%!" s Fmt.(list int) (List.map Uchar.to_int uchars);
                Lwt.return_unit
              | Ok () -> Lwt.return_unit
            ) l >>= fun _ ->
          Lwt.return @@ Ok ()
        )
end
