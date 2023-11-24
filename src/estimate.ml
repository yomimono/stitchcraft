let estimate file margin =
  let open Estimator in
  let pattern = Util.pattern_or_die file in

  let (materials, tools, substrate_cost) = compile pattern ~margin_inches:margin in
  let (substrate_w, substrate_h) = materials.fabric in
  let total_cost, total_seconds = totals materials in

  Printf.printf "aida cloth: %.02f by %.02f inches (including %.02f margin) - approximate cost: USD %.02G\n%!"
    substrate_w substrate_h margin substrate_cost;
  Format.printf "%a\n%!" pp_hoop_size tools.hoop_size;
  Format.printf "%a\n%!" pp_frame tools.frame_size;

  List.iter (fun thread -> Format.printf "%a\n" pp_thread_info thread) materials.threads;

  Printf.printf "total cost: %.02G; total time: %d seconds (%d minutes) (%.02G hours)\n%!"
    total_cost total_seconds (total_seconds / 60)
    ((float_of_int total_seconds) /. 3600.)
