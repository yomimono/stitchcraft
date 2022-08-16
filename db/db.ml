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
    let create = {|
    CREATE TABLE IF NOT EXISTS patterns (
        id bigserial PRIMARY KEY,
        name text NOT NULL,
        pattern json NOT NULL,
        tags bigint[]
    )
    |}

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
    let create = {|
    CREATE TABLE IF NOT EXISTS tags (
        id bigserial PRIMARY KEY,
        name text UNIQUE NOT NULL
    )
    |}

    let insert = {|
        INSERT INTO tags (name) VALUES ($1)
        ON CONFLICT DO NOTHING
    |}
      
    let names = {|
        WITH unnested_tags AS (select unnest(tags) from patterns) select name from unnested_tags join tags on unnest = tags.id;
      |}
  end

end
