(* give a summary of the materials needed, size when worked on various substrates, etc *)

let position ~font_size n =
  let f = int_of_float font_size in
  let vertical_offset = 1. *. 72. in
  let vertical_step n = (f + 4) * n |> float_of_int in
  Pdfops.(Op_cm (Pdftransform.matrix_of_transform [Pdftransform.Translate (72., vertical_offset +. vertical_step n)]))

let describe ti =
  let open Estimator in
  let name = Stitchy.DMC.Thread.to_string ti.thread in
  Format.asprintf "%s : %d stitches" name ti.amount

let pdfops_text ~font_size n s =
  let position = position ~font_size n in
  Pdfops.([
      Op_q;
      position;
      Font.helvetica_font_op font_size;
      Op_BT;
      Op_Tj s;
      Op_ET;
      Op_Q;
    ])

let pdfops_of_thread_info ~font_size n ti =
  pdfops_text ~font_size n (describe ti)

let total ~font_size position materials =
  let n_stitches = List.fold_left (fun acc ti -> acc + ti.Estimator.amount) 0 materials.Estimator.threads in
  let desc = Format.asprintf "%d stitches total" n_stitches in
  pdfops_text ~font_size position desc

let fabric_options ~font_size ~margin_inches start_position pattern =
  let describe_option size substrate =
    let open Stitchy.Types in
    let as_described = { substrate with grid = size} in
    let width, height = Estimator.substrate_size_in_inches ~margin_inches as_described in
    Format.asprintf "On Aida cloth count %a, the pattern will require %.02f x %.02f inches (with a %.01f inch margin)" Stitchy.Types.pp_grid size width height margin_inches
  in
  let ops position size = pdfops_text ~font_size position @@ describe_option size pattern.Stitchy.Types.substrate in
  let position = start_position in
  ops (position + 2) Stitchy.Types.Fourteen @ ops (position + 1) Stitchy.Types.Sixteen @ ops position Stitchy.Types.Eighteen

let create ~margin_inches ~font_size pattern =
  let materials = Estimator.materials ~margin_inches pattern in
  let stitch_sizes = List.mapi (fun n ti -> pdfops_of_thread_info ~font_size n ti) materials.threads |> List.flatten in
  let n_threads = List.length materials.threads in
  (* TODO: line breaks and pagination: the silent killers *)
  let fabric = fabric_options ~font_size ~margin_inches (n_threads + 1) pattern in
  stitch_sizes @ (total ~font_size n_threads materials) @ fabric
