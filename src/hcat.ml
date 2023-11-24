let go files output =
  let patterns = List.map Util.pattern_or_die files in
  match Stitchy.Operations.(perform Hcat patterns) with
  | Error (`Msg s) ->
    Format.eprintf "error concatenating: %s\n!" s; exit 1
  | Ok [] -> Format.eprintf "no patterns returned\n%!"; exit 1
  | Ok (bigpattern::_) ->
    Util.output_or_die bigpattern output
