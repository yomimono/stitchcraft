let uchars phrases = Textstitch.uchars_of_phrase (String.concat "\n" phrases)

let make_pattern strict font textcolor background gridsize phrases min_width min_height interline output =
  let font_txt = Yojson.Safe.from_file font in
  match Stitchy.Types.font_of_yojson font_txt with
  | Error _ as n -> n
  | Ok map ->
    let uchars = uchars phrases in
    let lookup letter = Stitchy.Types.UcharMap.find_opt letter map in
    match Textstitch.stitch strict lookup textcolor background gridsize uchars ~min_width ~min_height interline with
    | Error charlist ->
      Error (Format.asprintf "no glyphs found for uchars %a" Fmt.(list int) (List.map Uchar.to_int charlist))
    | Ok pattern ->
      let json = Stitchy.Types.pattern_to_yojson pattern in
      Util.stdout_or_file json output

let stitch strict font textcolor background gridsize phrase interline min_width min_height output =
  match (make_pattern strict font textcolor background gridsize phrase interline min_width min_height output) with
  | Ok () -> ()
  | Error e -> Format.eprintf "%s\n%!" e; exit 1
