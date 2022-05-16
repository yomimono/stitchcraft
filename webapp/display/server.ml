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

let try_get_pattern id =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  let find_pattern =
    let open Caqti_request.Infix in
    Caqti_type.int -->! Caqti_type.(tup2 string string) @:- {|
      SELECT name, pattern FROM patterns WHERE id = ?
    |}
  in
  try
    Db.collect_list find_pattern @@ int_of_string id >>= function
    | Ok [] -> Lwt.return @@ `Not_found
    | Error _ -> Lwt.return @@ `Error
    | Ok ((name, pattern)::_) ->
      Lwt.return @@ `Pattern (name, pattern)
  with
  | Failure _ | Invalid_argument _ -> Lwt.return @@ `Error

let try_serve_pattern id =
  fun (module Db : Caqti_lwt.CONNECTION) ->
    try_get_pattern id (module Db) >>= function
    | `Not_found -> Dream.respond ~code:404 ""
    | `Error -> Dream.respond ~code:500 ""
    | `Pattern (name, pattern) ->
      Dream.html ~code:200 @@ Template.display id ~name pattern

let extract_values key l =
  List.filter_map
     (fun (k, v) -> if String.(equal (lowercase_ascii k) (lowercase_ascii key))
       then Some v else None) l

let count_matching_tags =
  let open Caqti_request.Infix in
  string_array -->! Caqti_type.int @:-
  "SELECT count(id) FROM tags WHERE name = ANY(?)"

let ensure_tags =
  let open Caqti_request.Infix in
  string_array -->. Caqti_type.unit @:-
  "INSERT INTO tags (name) SELECT unnest($1::text[]) ON CONFLICT DO NOTHING"

let create request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  Dream.log "evaluating multipart request...";
  Dream.multipart ~csrf:false request >>= function
  | `Ok l -> begin
    let tags = extract_values "tag" l |> List.flatten |> List.map snd in
    match extract_values "name" l |> List.flatten |> List.map snd with
    | [] | _::_::_ -> Dream.respond ~code:400 "supply exactly one name"
    | name::[] ->
      Dream.log "name and tags are readable";
      match extract_values "pattern" l |> List.flatten |> List.map snd with
      | [] | _::_::_ -> Dream.respond ~code:400 "supply exactly one pattern"
      | file::[] ->
      Dream.log "size %d file was transmitted" (String.length file);
      match Yojson.Safe.from_string file |> Stitchy.Types.pattern_of_yojson with
      | Error _ -> Dream.respond ~code:400 "supply a valid pattern"
      | Ok pattern ->
        Dream.log "decoded a pattern; will try to save it as requested";
        let normalized_json = Stitchy.Types.pattern_to_yojson pattern |> Yojson.Safe.to_string in
        let make_pattern =
          {| INSERT INTO patterns (name, pattern, tags)
              SELECT ?, ?,
              (SELECT ARRAY (SELECT id FROM tags WHERE name = ANY (?)))
             RETURNING id
          |} in
        let insert =
          let open Caqti_request.Infix in
          Caqti_type.(tup3 string string string_array) -->! Caqti_type.int @:-
          make_pattern
        in
        Db.exec ensure_tags tags >>= function
        | Error s -> Dream.log "failure ensuring %d tags; aborting pattern creation" (List.length tags); 
          Dream.log "error: %a" Caqti_error.pp s;
          Dream.respond ~code:500 ""
        | Ok () ->
          Db.find insert (name, normalized_json, tags) >>= function
          | Ok id ->
            Dream.redirect request (Format.asprintf "/pattern/%d" id)
          | Error s ->
            let pp fmt params = Caqti_request.make_pp_with_param () fmt (insert, params) in
            Dream.log "error inserting pattern: %a" Caqti_error.pp s;
            Dream.respond ~code:500 ""
  end
  | _e -> Dream.respond ~code:400 "create form wasn't ok"

let search request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  (* searches are implicitly across tags and conjunctive *)
  Dream.form request >>= function
  | `Ok l -> begin
    let tags = extract_values "tag" l in
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
    Db.find count_matching_tags tags >>= function
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
    Dream.get "/pattern/new" (fun request -> Dream.respond @@ Template.upload request);
    Dream.post "/pattern/new" (fun request -> Dream.sql request @@ create request);
    Dream.get "/pattern/:id" (fun request -> Dream.sql request @@ (try_serve_pattern @@ Dream.param request "id"));
    Dream.post "/search" (fun request -> Dream.sql request @@ search request);
    Dream.get "/" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/index.html" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/display.js" @@ Dream.from_filesystem "" "display.bc.js";
    Dream.get "/tags.js" @@ Dream.from_filesystem "" "tags.bc.js";
  ]
