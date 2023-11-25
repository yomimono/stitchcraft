let go fn (file : string) =
  fn (Util.pattern_or_die file) |> 
    Stitchy.Types.pattern_to_yojson |> Yojson.Safe.to_channel stdout

let operate operation files output =
  let patterns = List.map Util.pattern_or_die files in
  match Stitchy.Operations.(perform operation patterns) with
  | Error (`Msg s) ->
    Format.eprintf "error operating: %s\n!" s; exit 1
  | Ok [] -> Format.eprintf "no patterns returned\n%!"; exit 1
  | Ok (bigpattern::_) ->
    Util.output_or_die bigpattern output

let hcenter amount files output =
  let operation = Stitchy.Operations.Hcenter amount in
  operate operation files output

let vcenter amount files output =
  let operation = Stitchy.Operations.Vcenter amount in
  operate operation files output
