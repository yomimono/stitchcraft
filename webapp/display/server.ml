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
    (* problem: I can't find a way to define a custom encoder/decoder that (1) allows me to input strings and (2) doesn't pass what I type through a `printf %S`.  So it's unclear to me how,
     * even with a custom encoder/decoder,
     * I would pass a string array? *)
    (* I can loop over the list of tags to get a list, although that seems kinda bonkers *)
    (* and what am I going to do with it once I have it? I still don't have a way to get it,
     * in list form, into a query.
     *
     * I think I need to either submit or carry a patch? *)
    let get_tag_id =
      Caqti_request.find_opt Caqti_type.string Caqti_type.int
        "SELECT id FROM tags WHERE name = ?"
    in
    Lwt_list.map_p (Db.find_opt get_tag_id) tags >>= fun tag_ids ->
    let find_tags =
      Caqti_request.collect Caqti_type.string Caqti_type.(tup2 int string)
        {|
        SELECT
        id, name
        FROM patterns
        WHERE tags @>
          (SELECT ARRAY
             (SELECT id FROM tags WHERE name = ANY(?)))
        |}
    in
    Db.collect_list find_tags tags >>= function
    | Ok [] -> Dream.respond ~code:404 "no results"
    | Ok l -> begin
      let links = List.map Template.link l |> String.concat "<br/>" in
      Dream.html ~code:200 links
      end
    | Error s -> Dream.html ~code:500 @@ Format.asprintf "%a" Caqti_error.pp s
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
