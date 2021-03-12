open Stitchy.Types
open Estimator

let pattern =
  let doc = "pattern to analyze for materials and cost. default is stdin" in
  Cmdliner.Arg.(value & pos 0 string "-" & info [] ~doc)

let margin =
  let doc = "unstitched margin size in inches. For framing, at least 1 inch is recommended." in
  Cmdliner.Arg.(value & opt float 1. & info ["margin";"m"] ~doc)

let estimate file margin =
  match Stitchy.Files.stdin_or_file file with
  | Error e -> failwith @@ Format.asprintf "input error: %s" e
  | Ok json ->
    match pattern_of_yojson json with
    | Error _ -> failwith "json parsing"
    | Ok pattern ->
      let materials = Estimator.materials pattern in
      let (substrate_w, substrate_h) = materials.fabric in
      let substrate_cost = substrate_w *. substrate_h *. aida_price_per_square_inch in
      let hoop_size = hoop_size pattern.substrate in
      Printf.printf "aida cloth: %.02f by %.02f inches (%.02f margin) - approximate cost: USD %.02G\n%!"
        substrate_w substrate_h margin substrate_cost;
      Format.printf "%a\n%!" pp_hoop_size hoop_size;
      List.iter print_thread_info materials.threads;
      let total_cost, total_seconds = totals materials.threads in
      Printf.printf "total cost: %.02G; total time: %d seconds (%d minutes) (%.02G hours)\n%!"
        total_cost total_seconds (total_seconds / 60)
        ((float_of_int total_seconds) /. 3600.)

let estimate_t = Cmdliner.Term.(const estimate $ pattern $ margin)

let info = Cmdliner.Term.info "calculate materials required for this pattern and estimate their cost"

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (estimate_t, info)
