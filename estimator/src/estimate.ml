open Stitchy.Types
open Estimator

let pattern =
  let doc = "pattern to analyze for materials and cost. - for stdin (the default)" in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let estimate file =
  match Stitchy.Files.stdin_or_file file with
  | Error e -> failwith @@ Format.asprintf "input error: %s" e
  | Ok json ->
    match state_of_yojson json with
    | Error _ -> failwith "json parsing"
    | Ok state ->
      let (substrate_w, substrate_h) = substrate_size_in_inches state.substrate in
      let substrate_cost = (float_of_int substrate_w) *. (float_of_int substrate_h) *. aida_price_per_square_inch in
      Printf.printf "aida cloth: %d by %d inches (approximate cost: USD %.02G)\n%!" substrate_w substrate_h substrate_cost;
      let threads = stitches_per_color state.stitches |> thread_info state.substrate.grid in
      List.iter print_thread_info threads;
      let total_cost, total_seconds = totals threads in
      Printf.printf "total cost: %.02G; total time: %d seconds (%d minutes) (%.02G hours)\n%!"
        total_cost total_seconds (total_seconds / 60)
        ((float_of_int total_seconds) /. 3600.)

let estimate_t = Cmdliner.Term.(const estimate $ pattern)

let info = Cmdliner.Term.info "calculate materials required for this pattern and estimate their cost"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (estimate_t, info)
