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
            <div id="grid"></div>
    </body>
  </html> |}

let to_pgsql_array l =
  let quoted_string fmt s = Format.fprintf fmt "\"%s\"" s in
  Ok (Format.asprintf "{%a}" Fmt.(list ~sep:comma quoted_string) l)

type _ Caqti_type.field +=
  | String_array : (string list) Caqti_type.field

let get_coding : type a. _ -> a Caqti_type.Field.t -> a Caqti_type.Field.coding = fun _ -> function
  | String_array ->
    let encode = to_pgsql_array
    and decode _ = Error "nope" (* TODO; for now we don't care *)
    in
    Caqti_type.Field.Coding {rep = Caqti_type.String; encode; decode}
  | _ -> assert false

let string_array = Caqti_type.field String_array

let try_serve_pattern id =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  let find_pattern =
    Caqti_request.find Caqti_type.int Caqti_type.string {|
      SELECT pattern FROM patterns WHERE id = ?
    |}
  in
  try
    Db.collect_list find_pattern @@ int_of_string id >>= function
    | Ok (pattern::_) ->
      Dream.json ~code:200 pattern
    | Ok [] ->
      Dream.respond ~code:404 ""
    | Error _ ->
      Dream.respond ~code:500 ""
  with
  | Invalid_argument _ -> Dream.respond ~code:400 ""

let search request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  (* searches are implicitly across tags and conjunctive *)
  (* TODO: currently csrf is false because we have no frontend,
   * so we're using curl for testing, but in the future it
   * should be true *)
  Dream.form ~csrf:false request >>= function
  | `Ok l -> begin
    let tags = List.filter_map
        (fun (k, v) -> if String.equal k "tag" then Some v else None) l
    in
    let tags_present =
      Caqti_request.find string_array Caqti_type.int
         "SELECT count(id) FROM tags WHERE name = ANY(?)"
    in
    let find_tags =
      Caqti_request.collect string_array Caqti_type.(tup2 int string)
        {|
        SELECT
        id, name
        FROM patterns
        WHERE tags @>
          (SELECT ARRAY
             (SELECT id FROM tags WHERE name = ANY(?)))
        |}
    in
    Db.find tags_present tags >>= function
    | Error _ -> Dream.html ~code:500 ""
    | Ok n when n <> List.length tags -> Dream.html ~code:400 ""
    | Ok _ ->
      Db.collect_list find_tags tags >>= function
      | Error s -> Dream.html ~code:500 @@ Format.asprintf "%a" Caqti_error.pp s
      | Ok [] -> Dream.respond ~code:404 "no results"
      | Ok l -> begin
          let links = List.map Template.link l |> String.concat "<br/>" in
          Dream.html ~code:200 links
        end
  end
  | _ -> Dream.respond ~code:400 ""



let () =
  Caqti_type.Field.define_coding String_array {get_coding};
  Dream.run @@ Dream.logger @@ Dream.sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ Dream.router [
    Dream.get "/pattern/:id" (fun request -> Dream.sql request @@ (try_serve_pattern @@ Dream.param request "id"));
    Dream.post "/search" (fun request -> Dream.sql request @@ search request);
    Dream.get "/" (fun _request -> Dream.respond ~code:200 html);
    Dream.get "/index.html" (fun _request -> Dream.respond ~code:200 html);
    Dream.get "/grid.js" @@ Dream.from_filesystem "" "grid.bc.js";
  ]
