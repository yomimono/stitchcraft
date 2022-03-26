open Stitchy
open Types
open Lwt.Infix

module Js = Js_of_ocaml.Js
module CSS = Js_of_ocaml.CSS
module Dom = Js_of_ocaml.Dom

let thread_to_css thread = CSS.Color.hex_of_rgb (DMC.Thread.to_rgb thread)

module Html = Js_of_ocaml.Dom_html

let create_canvas pattern =
  let d = Html.window##.document in
  let c = Html.createCanvas d in
  let w = Html.window##.innerWidth
  and h = Html.window##.innerHeight
  in
  let canvas_largest = max w h in
  let pattern_largest = (max pattern.substrate.max_x pattern.substrate.max_y) + 1 in
  let block_size = canvas_largest / pattern_largest in
  c##.width := block_size * (pattern.substrate.max_x + 1);
  c##.height := block_size * (pattern.substrate.max_y + 1);
  {Canvas.canvas = c; block_size}

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

let start _event =
  Lwt.ignore_result (
    load_pattern "json" >|= function
    | Error s ->
      let d = Html.window##.document in
      let error_element = Html.getElementById "error" in
      let err = d##createTextNode (Js.string s) in
      Dom.appendChild error_element err;
      Lwt.return_unit
    | Ok pattern ->
      let canvas = create_canvas pattern in
      Canvas.render canvas pattern;
      let grid = Html.getElementById "grid" in
      Dom.appendChild grid Canvas.(canvas.canvas);
      Lwt.return_unit
  );
  Js._false (* "If the handler returns false, the default action is prevented" *)

let () =
  Html.window##.onload := Html.handler start
