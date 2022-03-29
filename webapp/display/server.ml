open Lwt.Infix

let to_pgsql_array l =
  let quoted_string fmt s = Format.fprintf fmt "\"%s\"" s in
  Ok (Format.asprintf "{%a}" Fmt.(list ~sep:comma quoted_string) l)

type _ Caqti_type.field +=
  | String_array : (string list) Caqti_type.field

let get_coding : type a. _ -> a Caqti_type.Field.t -> a Caqti_type.Field.coding = fun _ -> function
  | String_array ->
    let encode = to_pgsql_array
    and decode _ = Error "string array decoding is not implemented" (* TODO; for now we don't care *)
    in
    Caqti_type.Field.Coding {rep = Caqti_type.String; encode; decode}
  | _ -> assert false

let string_array = Caqti_type.field String_array

let try_serve_pattern id =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  let find_pattern =
    let open Caqti_request.Infix in
    Caqti_type.int -->! Caqti_type.string @:- {|
      SELECT pattern FROM patterns WHERE id = ?
    |}
  in
  try
    Db.collect_list find_pattern @@ int_of_string id >>= function
    | Ok (pattern::_) ->
      Dream.html ~code:200 @@ Template.display pattern
    | Ok [] ->
      Dream.respond ~code:404 ""
    | Error _ ->
      Dream.respond ~code:500 ""
  with
  | Failure _ | Invalid_argument _ -> Dream.respond ~code:400 ""

let search request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  (* searches are implicitly across tags and conjunctive *)
  Dream.form request >>= function
  | `Ok l -> begin
    let tags = List.filter_map
        (fun (k, v) -> if String.equal k "tag" then Some v else None) l
    in
    let tags_present =
      let open Caqti_request.Infix in
      string_array -->! Caqti_type.int @:-
         "SELECT count(id) FROM tags WHERE name = ANY(?)"
    in
    let find_tags =
      let open Caqti_request.Infix in
      string_array -->* Caqti_type.(tup4 int string int int) @:-
        {|
        SELECT
        id, name, pattern->'substrate'->'max_x', pattern->'substrate'->'max_y'
        FROM patterns
        WHERE tags @>
          (SELECT ARRAY
             (SELECT id FROM tags WHERE name = ANY(?)))
        |}
    in
    Db.find tags_present tags >>= function
    | Error _ -> Dream.html ~code:500 "error finding tags"
    | Ok n when n <> List.length tags -> Dream.respond ~code:400 
                                           (Format.asprintf
                                              "only %d tags of %d were found" n (List.length tags))
    | Ok _ ->
      Db.collect_list find_tags tags >>= function
      | Error s -> Dream.html ~code:500 @@ Format.asprintf "%a" Caqti_error.pp s
      | Ok [] -> Dream.respond ~code:404 "no results"
      | Ok l -> begin
          let links = List.map Template.link l |> String.concat "<br/>" in
          Dream.html ~code:200 links
        end
  end
  | _e -> Dream.respond ~code:400 "form wasn't ok"



let () =
  Caqti_type.Field.define_coding String_array {get_coding};
  Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ Dream.sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ Dream.router [
    Dream.get "/pattern/:id" (fun request -> Dream.sql request @@ (try_serve_pattern @@ Dream.param request "id"));
    Dream.post "/search" (fun request -> Dream.sql request @@ search request);
    Dream.get "/" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/index.html" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/display.js" @@ Dream.from_filesystem "" "display.bc.js";
    Dream.get "/tags.js" @@ Dream.from_filesystem "" "tags.bc.js";
  ]
