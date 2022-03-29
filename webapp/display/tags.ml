open Js_of_ocaml

let add_tag_field _event =
  let d = Dom_html.window##.document in
  let div = Dom_html.getElementById "new_tags_zone" in
  let label = Dom_html.createLabel d in
  Dom.appendChild label (d##createTextNode (Js.string "Tag:"));
  let t = Dom_html.createInput ~_type:(Js.string "text") ~name:(Js.string "tag") Dom_html.document in
  t##.disabled := Js.bool false;
  t##.required := Js.bool true;
  Dom.appendChild label t;
  Dom.appendChild div label;
  Dom.appendChild div @@ Dom_html.createBr d;
  Js._true

let tag_adding_handler _ =
  let button = Dom_html.getElementById "add_tags" in
  button##.onclick := Dom_html.handler add_tag_field;
  Js._true

let () =
  Dom_html.window##.onload := Dom_html.handler tag_adding_handler
