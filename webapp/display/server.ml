open Lwt.Infix
let html = {|
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
          "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Pixel Canvas!</title>
    <script type="text/javascript" src="grid.js"></script>
  </head>
  <body>
          <div id="error"></div>
          <div id="drawbox"></div>
          <div id="colorbar"></div>
          <div id="download"></div>
          <div id="upload"></div>
          <div id="json"></div>
          <div id="library"></div>
  </body>
</html>
  |}
 
let try_serve_pattern id =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  let find_pattern =
    Caqti_request.find Caqti_type.int Caqti_type.string {|
      SELECT pattern FROM patterns WHERE id = ?
    |}
  in
  try
    Db.collect_list find_pattern @@ int_of_string id >>= function
    | Ok (_pattern::_) ->
      (* Dream.json ~code:200 pattern *)
      Dream.html ~code:200 html
    | Ok [] ->
      Dream.respond ~code:404 ""
    | Error _ ->
      Dream.respond ~code:500 ""
  with
  | Invalid_argument _ -> Dream.respond ~code:400 ""

let () =
  Dream.run @@ Dream.logger @@ Dream.sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ Dream.router [
    Dream.get "/pattern/:id" (fun request -> Dream.sql request @@ (try_serve_pattern @@ Dream.param request "id"));
    Dream.get "/" (fun _request -> Dream.respond ~code:200 html);
    Dream.get "/grid.js" @@ Dream.from_filesystem "" "grid.bc.js";
  ]
