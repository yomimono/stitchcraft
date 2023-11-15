open Stitchy
open Types
open Lwt.Infix

module Js = Js_of_ocaml.Js
module CSS = Js_of_ocaml.CSS
module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml.Dom_html

module Canvas = Js_canvas.Canvas

let thread_to_css thread = CSS.Color.hex_of_rgb (DMC.Thread.to_rgb thread)

let create_canvas pattern ~max_width ~max_height =
  let d = Html.window##.document in
  let html_canvas = Html.createCanvas d in
  let w = max_width
  and h = max_height
  in
  let canvas_smallest = max 1 (min w h) in
  let pattern_largest = (max pattern.substrate.max_x pattern.substrate.max_y) + 1 in
  let block_size = canvas_smallest / pattern_largest in
  html_canvas##.width := block_size * (pattern.substrate.max_x + 1);
  html_canvas##.height := block_size * (pattern.substrate.max_y + 1);
  Canvas.({canvas = html_canvas; block_size})

let load_pattern tag =
  let d = Html.window##.document in
  let json =
    match Js.Opt.to_option @@ d##getElementById (Js.string tag) with
    | None -> "{}"
    | Some e -> match Js.Opt.to_option e##.textContent with
      | None -> "{}"
      | Some e -> Js.to_string e
  in
  match Yojson.Safe.from_string json with
  | exception Stack_overflow -> Lwt.return @@ Error "this string is too big to parse as json"
  | s ->
  match pattern_of_yojson s with
  | Ok state -> Lwt.return @@ Ok state
  | Error s -> Lwt.return @@ Error (Format.asprintf "error getting state from valid json: %s" s)
  | exception (Yojson.Json_error s) ->
    Lwt.return @@ Error (Format.asprintf "error parsing json: %s" s)
  | exception Stack_overflow ->
    Lwt.return @@ Error "this JSON is too big to parse as a pattern"

let ul_of_threads (threads : Estimator.thread_info list) =
  let li_of_thread (thread : Estimator.thread_info) =
    let li = Html.createLi Html.window##.document in

    let b = Html.createB Html.window##.document in
    let p = Html.createP Html.window##.document in
    let name = Stitchy.DMC.Thread.to_string thread.thread in
    p##.textContent := Js.Opt.return (Js.string @@ Format.asprintf "%d stitches (%.02f linear inches, %d standard skein(s))" thread.amount thread.length @@ int_of_float @@ Float.ceil thread.skeins );
    b##.textContent := Js.Opt.return (Js.string name);
    List.iter (Dom.appendChild li) [b; p];
    li
  in
  let alphabetize = List.sort (fun (a : Estimator.thread_info) b -> Stitchy.DMC.Thread.compare a.thread b.thread) in
  let ul = Html.createUl Html.window##.document in
  let lis = List.map li_of_thread (alphabetize threads) in
  List.iter (Dom.appendChild ul) lis;
  ul

let pp_time fmt = function
  | n when n > 60 * 60 * 24 -> Format.fprintf fmt "%d hours" (n / 60 / 60)
  | n when n > 60 * 60 -> Format.fprintf fmt "%d minutes" (n / 60)
  | n -> Format.fprintf fmt "%d seconds" n

let start _event =
  let margin_inches = 1. in
  Lwt.ignore_result (
    let d = Html.window##.document in
    load_pattern "json" >|= function
    | Error s ->
      let error_element = Html.getElementById "error" in
      let err = d##createTextNode (Js.string s) in
      Dom.appendChild error_element err;
      Lwt.return_unit
    | Ok pattern ->
      let grid = Html.getElementById "grid"
      and thread = Html.getElementById "thread"
      in
      let max_width = Html.window##.innerWidth
      and max_height = Html.window##.innerHeight
      in
      let canvas = create_canvas ~max_width ~max_height pattern in
      Canvas.render canvas pattern;
      Dom.appendChild grid Canvas.(canvas.canvas);
      let bill_of_materials = Estimator.materials ~margin_inches pattern in
      (* TODO: don't hardcode 14-count Aida everywhere *)
      let fabric = Js.string @@ Format.asprintf "a %.02f in. x %.02f in. piece of %d-count Aida cloth (including %.02f in. margin on every side left blank for mounting)" (fst bill_of_materials.fabric) (snd bill_of_materials.fabric) 14 margin_inches in
      let ul = ul_of_threads bill_of_materials.threads in
      let total = Estimator.totals bill_of_materials in
      let summary = d##createTextNode (Js.string @@ Format.asprintf "Estimated total: %.2f USD, %a" (fst total) pp_time (snd total)) in
      let fabric = d##createTextNode fabric in
      Dom.appendChild (Html.getElementById "summary") summary;
      Dom.appendChild thread ul;
      Dom.appendChild (Html.getElementById "fabric") fabric;
      Lwt.return_unit
  );
  Js._false (* "If the handler returns false, the default action is prevented" *)

let () =
  Html.window##.onload := Html.handler start
