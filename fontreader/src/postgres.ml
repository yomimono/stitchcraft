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
        CONSTRAINT positive CHECK (height > 0 AND width > 0)
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

let new_font connection name =
  let param = Pgx.Value.of_string name in
  Pgx_lwt_unix.execute connection ~params:[param]
    "INSERT INTO FONTS (name) VALUES ($1) ON CONFLICT (name) DO NOTHING RETURNING id"
  >>= function
  | [] ->
    (* TODO it'd be nice to have an overwrite/update option *)
    Lwt.return @@ Error (`Msg (Format.asprintf "a font named %s already exists" name))
  | _::_::_ -> Lwt.return @@ Error (`Msg "made more than one font")
  | row::[] ->
    match row with
    | id::[] -> Lwt.return @@ Option.to_result ~none:(`Msg "font id wasn't an int somehow")
      @@ Pgx.Value.to_int id
    | _ -> Lwt.return @@ Error (`Msg "new font creation didn't return an id")

let associate_glyph ~glyph_id ~font_id connection uchars =
  let query = "INSERT INTO fonts_glyphs (font, uchar, glyph) VALUES ($1, $2, $3)" in
  let params uchar =
    [Pgx.Value.of_int font_id;
     Pgx.Value.of_int (Uchar.to_int uchar);
     Pgx.Value.of_int glyph_id] in
  Pgx_lwt_unix.execute_many connection ~params:(List.map params uchars) ~query >>= function
  | l when not @@ Int.equal (List.length l) (List.length uchars) ->
    Lwt.return @@ Error (`Msg (Format.asprintf "fewer association rows than expected were created for glyph %d" glyph_id))
  | _ -> Lwt.return @@ Ok ()

let new_glyph connection (Stitchy.Types.{width; height; stitches}) =
  let stitches_json = Stitchy.Types.CoordinateSet.to_yojson stitches in
  let stitches_string = Yojson.Safe.to_string stitches_json in
  let params = [Pgx.Value.of_int width; Pgx.Value.of_int height;
                Pgx.Value.of_string stitches_string] in
  let query =
    "INSERT INTO glyphs (width, height, stitches) VALUES ($1, $2, $3) RETURNING id"
  in
  Pgx_lwt_unix.execute connection ~params query >>= function
  | [] | _::_::_ -> Lwt.return @@ Error (`Msg "failed to insert a glyph")
  | row::[] ->
    match row with
    | id::[] -> Lwt.return @@ Option.to_result ~none:(`Msg "glyph id wasn't an int somehow")
      @@ Pgx.Value.to_int id
    | _ -> Lwt.return @@ Error (`Msg "new glyph creation didn't return an id")

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
          new_font connection font >>= function
          | Error _ as e -> Lwt.return e
          | Ok font_id ->
            if debug then Format.printf "created new font at font_id %d\n%!" font_id;
            Lwt_list.iter_p (fun (glyph, uchars) ->
              new_glyph connection glyph >>= function
              | Error (`Msg s) ->
                Format.eprintf "Error %s: couldn't insert glyph for %a; skipping those uchars\n%!" s Fmt.(list int) (List.map Uchar.to_int uchars);
                Lwt.return_unit
              | Ok glyph_id -> begin
                associate_glyph ~glyph_id ~font_id connection uchars >>= function
                | Error (`Msg s) -> Format.eprintf "Error %s: couldn't associate glyphs for %a" s Fmt.(list int) (List.map Uchar.to_int uchars);
                  Lwt.return_unit
                | Ok _ -> Lwt.return_unit
              end
            ) l >>= fun _ ->
            Lwt.return @@ Ok ()
        )
end
