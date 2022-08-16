(* Caqti types are now also allowed to be string arrays *)
(* We include this at the top-level because it needs to be evaluated by our users *)
type _ Caqti_type.field +=
  | String_array : (string list) Caqti_type.field

let string_array = Caqti_type.field String_array

let string_array_to_pgsql_array l =
  let quoted_string fmt s = Format.fprintf fmt "\"%s\"" s in
  Ok (Format.asprintf "{%a}" Fmt.(list ~sep:comma quoted_string) l)

let get_coding : type a. _ -> a Caqti_type.Field.t -> a Caqti_type.Field.coding = fun _ -> function
  | String_array ->
    let encode = string_array_to_pgsql_array
    and decode _ = Error "string array decoding is not implemented" (* TODO; for now we don't care *)
    in
    Caqti_type.Field.Coding {rep = Caqti_type.String; encode; decode}
  | _ -> assert false

let () = Caqti_type.Field.define_coding String_array {get_coding}

module CLI = struct

  type db = { host : string;
              port : int;
              database : string;
              user : string;
              password : string;
            }

  let set_db host port database user password = {host; port; database; user; password }

  let db_t =
    let host =
      let doc = "postgresql server hostname" in
      let env = Cmdliner.Cmd.Env.info "PGHOST" in
      Cmdliner.Arg.(value & opt string "localhost" & info ["h"; "host"] ~docv:"PGHOST" ~doc ~env)
    in
    let port =
      let doc = "postgresql server port" in
      let env = Cmdliner.Cmd.Env.info "PGPORT" in
      Cmdliner.Arg.(value & opt int 5432 & info ["p"; "port"] ~docv:"PGPORT" ~doc ~env)
    in
    let database =
      let doc = "postgresql database name" in
      let env = Cmdliner.Cmd.Env.info "PGDATABASE" in
      Cmdliner.Arg.(value & opt string "stitchcraft" & info ["db"] ~docv:"PGDATABASE" ~doc ~env)
    in
    let user =
      let doc = "postgresql user" in
      let env = Cmdliner.Cmd.Env.info "PGUSER" in
      Cmdliner.Arg.(value & opt string "stitchcraft" & info ["u"] ~docv:"PGUSER" ~doc ~env)
    in
    let password =
      let doc = "postgresql user's password" in
      let env = Cmdliner.Cmd.Env.info "PGPASSWORD" in
      Cmdliner.Arg.(value & opt string "s3kr1t" & info ["pass"] ~docv:"PGPASSWORD" ~doc ~env)
    in
    Cmdliner.Term.(const set_db $ host $ port $ database $ user $ password)
end

module ORM = struct
  open Caqti_request.Infix
  module Glyphs = struct
    let create = {|
      CREATE TABLE IF NOT EXISTS glyphs (
        id              bigserial PRIMARY KEY,
        height          integer NOT NULL,
        width           integer NOT NULL,
        stitches        json NOT NULL,
        CONSTRAINT positive CHECK (height >= 0 AND width >= 0)
        )
      |}

    let query =  {|WITH font_id AS (
    SELECT id FROM fonts WHERE name=$1
    ), glyph_id AS (
    SELECT glyph FROM fonts_glyphs
    JOIN font_id on font_id.id = fonts_glyphs.font
    AND fonts_glyphs.uchar = $2
    )
    SELECT $2, width, height, stitches FROM glyphs
    INNER JOIN glyph_id ON glyph_id.glyph = glyphs.id
    |}

  end

  module Fonts = struct
    let create = {|
        CREATE TABLE IF NOT EXISTS fonts (
        id      bigserial PRIMARY KEY,
        name    text NOT NULL,
        CONSTRAINT unique_name UNIQUE(name)
        )
      |}
  end

  module Fonts_Glyphs = struct
    let create = {|
    CREATE TABLE IF NOT EXISTS fonts_glyphs (
        font integer references fonts(id),
        uchar integer NOT NULL,
        glyph integer references glyphs(id),
        CONSTRAINT positive CHECK (uchar >= 0)
        )
    |}

    let insert = {|
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


  end

  module Patterns = struct
    let create = Caqti_type.unit ->. Caqti_type.unit @@ {|
    CREATE TABLE IF NOT EXISTS patterns (
        id bigserial PRIMARY KEY,
        name text NOT NULL,
        pattern json NOT NULL,
        tags bigint[]
    )
    |}

    let insert_with_tags (module Caqti_db : Caqti_lwt.CONNECTION) name tags pattern =
      let make_pattern =
        {| INSERT INTO patterns (name, pattern, tags)
            SELECT ?, ?,
            (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY (?)))
           RETURNING id
        |} in
      let insert =
        let open Caqti_request.Infix in
        Caqti_type.tup3 Caqti_type.string Caqti_type.string string_array -->!
        Caqti_type.int @:-
        make_pattern
      in
      let normalized_json = Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_string in
      Caqti_db.find insert (name, normalized_json, tags)

    let insert = {|
      INSERT INTO patterns (name, pattern, tags)
      SELECT $1, $2, (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY ($3) ))
      |}

    let search_by_tag = {|
      SELECT id, name, pattern, tags
      FROM patterns
      WHERE tags @> (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY($1)))
      |}

  end

  module Tags = struct
    let create = Caqti_type.unit ->. Caqti_type.unit @@ {|
    CREATE TABLE IF NOT EXISTS tags (
        id bigserial PRIMARY KEY,
        name text UNIQUE NOT NULL
    )
    |}

    let insert = {|
        INSERT INTO tags (name) SELECT unnest($1::text[]) ON CONFLICT DO NOTHING
    |}
      
    let names = {|
        WITH unnested_tags AS (select unnest(tags) from patterns) select name from unnested_tags join tags on unnest = tags.id;
      |}

    let find =
      let open Caqti_request.Infix in
      string_array -->* Caqti_type.(tup4 int string int int) @:-
      {|
        SELECT
        id, name, pattern->'substrate'->'max_x', pattern->'substrate'->'max_y'
        FROM patterns
        WHERE tags @>
          (SELECT ARRAY
             (SELECT id FROM tags WHERE name = ANY(?)))
      |}


  end

end
