let replace input output src dst =
  match Util.pattern_of_input input with
  | Error e -> failwith e
  | Ok pattern ->
    let p = Stitchy.Operations.replace_thread ~src ~dst pattern in
    match Util.pattern_to_output p output with
    | Error e -> failwith e
    | Ok _ -> ()
