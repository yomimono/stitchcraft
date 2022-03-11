open Lwt.Infix

let make_db connection =
  Pgx_lwt_unix.execute connection {|
    CREATE TABLE IF NOT EXISTS glyphs (
        id              integer PRIMARY KEY,
        height          integer NOT NULL,
        width           integer NOT NULL,
        stitches        json NOT NULL
        CONSTRAINT con1 CHECK (height > 0 AND width > 0)
        )
    |}
