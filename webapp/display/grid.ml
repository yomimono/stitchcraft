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

let load_pattern id =
  Js_of_ocaml_lwt.XmlHttpRequest.get (Format.asprintf "/pattern/%d" id) >>= fun response ->
  match response.Js_of_ocaml_lwt.XmlHttpRequest.code with
  | 200 -> begin
      match pattern_of_yojson @@ Yojson.Safe.from_string response.Js_of_ocaml_lwt.XmlHttpRequest.content with
      | Ok state -> Lwt.return @@ Ok state
      | Error s -> Lwt.return @@ Error (Format.asprintf "error getting state from valid json: %s" s)
      | exception (Yojson.Json_error s) ->
        Lwt.return @@ Error (Format.asprintf "error parsing json: %s" s)
    end
  | n ->
    Lwt.return @@ Error (Format.asprintf "failed to get library: code %d, content %s" n
             response.Js_of_ocaml_lwt.XmlHttpRequest.content)

let start _event =
  Lwt.ignore_result (
    load_pattern 2 >|= function
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
