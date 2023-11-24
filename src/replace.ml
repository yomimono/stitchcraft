let replace input output src dst =
  let pattern = Util.pattern_or_die input in
  let p = Stitchy.Operations.replace_thread ~src ~dst pattern in
  Util.output_or_die p output
