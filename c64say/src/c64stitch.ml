open Stitchy.Types
(* take a phrase, output a stitch file *)

let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])
let interline =
  let doc = "extra space to insert between lines (in stitches)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output =
  let doc = "file to output json to. -, the default, is stdout" in
  Cmdliner.Arg.(value & opt string "-" & info ["o"; "output"] ~doc)

let textcolor =
  let thread_conv = Cmdliner.Arg.conv Stitchy.DMC.Thread.(parse, pp) in
  let default = List.hd Stitchy.DMC.Thread.basic in
  let doc = "thread identifier for text" in
  Cmdliner.Arg.(value & opt thread_conv default & info ["t"; "textcolor"] ~doc)

let bgcolor =
  let doc = "color of background" in
  Cmdliner.Arg.(value & opt (t3 int int int) (255, 255, 255) & info ["b";"bg";"background"] ~doc)

let grid_converter : (string * Stitchy.Types.grid) list = [
  "14", Fourteen;
  "16", Sixteen;
  "18", Eighteen;
]

let gridsize =
  let doc = "size of aida cloth grid" in
  Cmdliner.Arg.(value & opt (enum grid_converter) Stitchy.Types.Fourteen & info ["g"; "gridsize"] ~doc)

let font_name =
  let doc = "font to use (should match a database name)" in
  let env = Cmdliner.Cmd.Env.info "STITCH_FONT" ~doc in
  Cmdliner.Arg.(value & opt string "Bm437_PhoenixEGA_8x8" & info ~env ["f"; "font"] ~doc ~docv:"FONT")

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

let make_pattern font {host; port; database; password; user } textcolor background gridsize phrase interline output =
  let open Lwt.Infix in
  let uchars = C64say.Assemble.uchars_of_phrase phrase in
  Pgx_lwt_unix.connect ~host ~port ~database ~password ~user () >>= fun connection ->
  let to_lookup = List.sort_uniq Uchar.compare uchars in
  let params = List.map (fun u -> Pgx.Value.[of_string font; of_int (Uchar.to_int u)]) to_lookup in
  let query = {|WITH font_id AS (
    SELECT id FROM fonts WHERE name=$1
    ), glyph_id AS (
    SELECT glyph FROM fonts_glyphs
    JOIN font_id on font_id.id = fonts_glyphs.font
    AND fonts_glyphs.uchar = $2
    )
    SELECT $2, width, height, stitches FROM glyphs
    INNER JOIN glyph_id ON glyph_id.glyph = glyphs.id
    |} in
  Pgx_lwt_unix.execute_many connection ~params ~query >>= fun rows ->
  let map = List.fold_left (fun map r ->
      match r with
      | u::w::h::s::[] -> begin
        match Pgx.Value.to_string s with
          | None -> map
          | Some json ->
            match Stitchy.Types.CoordinateSet.of_yojson @@ Yojson.Safe.from_string json with
            | Error _ -> map
            | Ok stitches ->
              let width = Pgx.Value.to_int_exn w
              and height = Pgx.Value.to_int_exn h
              and uchar = Pgx.Value.to_int_exn u |> Uchar.of_int 
              in
              let glyph = Stitchy.Types.({width; height; stitches}) in
              UcharMap.add uchar glyph map
      end
      | _ -> map
    ) Stitchy.Types.UcharMap.empty (List.flatten rows)
  in
  let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
  let pattern = C64say.Assemble.stitch lookup textcolor background gridsize uchars interline in
  let json = Stitchy.Types.pattern_to_yojson pattern in
  Lwt.return @@ Stitchy.Files.stdout_or_file json output

let stitch font db textcolor background gridsize phrase interline output =
  Lwt_main.run @@ make_pattern font db textcolor background gridsize phrase interline output

let stitch_t = Cmdliner.Term.(const stitch $ font_name $ db_t $ textcolor $ bgcolor $ gridsize $ phrase $ interline $ output)

let info =
  let doc = "make a stitch file repesenting a phrase in a known font" in
  Cmdliner.Cmd.info "c64stitch" ~doc

let () =
  exit @@ Cmdliner.Cmd.eval_result @@ Cmdliner.Cmd.v info stitch_t
