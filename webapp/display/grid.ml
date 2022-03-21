open Stitchy
open Types
open Lwt.Infix

module Js = Js_of_ocaml.Js
module CSS = Js_of_ocaml.CSS
module Dom = Js_of_ocaml.Dom

let thread_to_css thread = CSS.Color.hex_of_rgb (DMC.Thread.to_rgb thread)

module Html = Js_of_ocaml.Dom_html

let create_canvas width height =
  let d = Html.window##.document in
  let c = Html.createCanvas d in
  c##.width := width;
  c##.height := height;
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
  Lwt.ignore_result (
    load_pattern 1 >|= function
    | Error s ->
      let d = Html.window##.document in
      let error_element = Html.getElementById "error" in
      let err = d##createTextNode (Js.string s) in
      Dom.appendChild error_element err;
      Lwt.return_unit
    | Ok pattern ->
      let canvas = create_canvas
          Stitchy.Types.((pattern.substrate.max_x + 1) * Canvas.block_size)
          Stitchy.Types.((pattern.substrate.max_y + 1) * Canvas.block_size)
      in
      Canvas.render canvas pattern;
      let drawbox = Html.getElementById "drawbox" in
      Dom.appendChild drawbox canvas;
      Lwt.return_unit
  );
  Js._false (* "If the handler returns false, the default action is prevented" *)

let () =
  Html.window##.onload := Html.handler start
