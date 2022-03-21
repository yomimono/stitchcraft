open Stitchy
open Types
open Lwt.Infix

module Js = Js_of_ocaml.Js
module CSS = Js_of_ocaml.CSS
module Dom = Js_of_ocaml.Dom

let block_size = 20
let thread_to_css thread = CSS.Color.hex_of_rgb (DMC.Thread.to_rgb thread)

let init =
  let substrate = {
    background = 255, 255, 255;
    grid = Fourteen;
    max_x = 19;
    max_y = 19;
  } in
  { substrate; layers = []; backstitch_layers = [] }

module Html = Js_of_ocaml.Dom_html

let create_canvas size =
  let d = Html.window##.document in
  let c = Html.createCanvas d in
  c##.width := size;
  c##.height := size;
  c

let load_pattern id =
  Js_of_ocaml_lwt.XmlHttpRequest.get (Format.asprintf "/pattern/%d" id) >>= fun response ->
  match response.Js_of_ocaml_lwt.XmlHttpRequest.code with
  | 200 -> begin
      (* see whether we can translate that json into a pattern *)
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
  (* TODO: figure out a sensible size for the canvas based on window size *)
  (* TODO: also figure out a sensible size for block display *)
  let canvas = create_canvas 300 in
  let state = ref init in
  Lwt.ignore_result (
    load_pattern 1 >|= function
    | Error s ->
      let d = Html.window##.document in
      let error_element = Html.getElementById "error" in
      let err = d##createTextNode (Js.string s) in
      Dom.appendChild error_element err;
      Lwt.return_unit
    | Ok pattern -> state := pattern; Lwt.return_unit
  );
  Canvas.render canvas !state;
  let drawbox = Html.getElementById "drawbox" in
  Dom.appendChild drawbox canvas;
  Js._false (* "If the handler returns false, the default action is prevented" *)

let () =
  Html.window##.onload := Html.handler start
