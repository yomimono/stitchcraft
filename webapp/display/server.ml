open Lwt.Infix

let try_get_pattern id =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
  try
    Caqti_db.collect_list Db.ORM.Patterns.get_by_id @@ int_of_string id >>= function
    | Ok [] -> Lwt.return @@ `Not_found
    | Error _ -> Lwt.return @@ `Error
    | Ok ((name, pattern)::_) ->
      Lwt.return @@ `Pattern (name, pattern)
  with
  | Failure _ | Invalid_argument _ -> Lwt.return @@ `Error

let try_serve_pattern id =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
    try_get_pattern id (module Caqti_db) >>= function
    | `Not_found -> Dream.respond ~code:404 ""
    | `Error -> Dream.respond ~code:500 ""
    | `Pattern (name, pattern) ->
      Dream.html ~code:200 @@ Template.display id ~name pattern

let extract_values key l =
  List.filter_map
     (fun (k, v) -> if String.(equal (lowercase_ascii k) (lowercase_ascii key))
       then Some v else None) l

let create request =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
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
        Caqti_db.exec Db.ORM.Tags.insert tags >>= function
        | Error s -> Dream.log "failure ensuring %d tags; aborting pattern creation" (List.length tags); 
          Dream.log "error: %a" Caqti_error.pp s;
          Dream.respond ~code:500 ""
        | Ok () ->
          Caqti_db.find Db.ORM.Patterns.insert_with_tags (name, normalized_json, tags) >>= function
          | Ok id ->
            Dream.redirect request (Format.asprintf "/pattern/%d" id)
          | Error s ->
            Dream.log "error inserting pattern: %a" Caqti_error.pp s;
            Dream.respond ~code:500 ""
  end
  | _e -> Dream.respond ~code:400 "create form wasn't ok"

let search request =
  fun (module Caqti_db : Caqti_lwt.CONNECTION) ->
  (* searches are implicitly across tags and conjunctive *)
  Dream.form request >>= function
  | `Ok l -> begin
    let tags = extract_values "tag" l in
    Caqti_db.find Db.ORM.Tags.count tags >>= function
    | Error _ -> Dream.html ~code:500 "error finding tags"
    | Ok n when n <> List.length tags -> Dream.respond ~code:400 
                                           (Format.asprintf
                                              "only %d tags of %d were found" n (List.length tags))
    | Ok _ ->
      Caqti_db.collect_list Db.ORM.Tags.find tags >>= function
      | Error s -> Dream.html ~code:500 @@ Format.asprintf "%a" Caqti_error.pp s
      | Ok [] -> Dream.respond ~code:404 "no results"
      | Ok l -> begin
          let links = List.map Template.link l |> String.concat "<br/>" in
          Dream.html ~code:200 links
        end
  end
  | _e -> Dream.respond ~code:400 "form wasn't ok"


let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ Dream.sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ Dream.router [
    Dream.get "/pattern/new" (fun request -> Dream.respond @@ Template.upload request);
    Dream.post "/pattern/new" (fun request -> Dream.sql request @@ create request);
    Dream.get "/pattern/:id" (fun request -> Dream.sql request @@ (try_serve_pattern @@ Dream.param request "id"));
    Dream.post "/search" (fun request -> Dream.sql request @@ search request);
    Dream.get "/" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/index.html" (fun request -> Dream.respond ~code:200 @@ Template.index request);
    Dream.get "/create" (fun request -> Dream.respond ~code:200 @@ Template.create request);
    Dream.get "/display.js" @@ Dream.from_filesystem "" "display.bc.js";
    Dream.get "/tags.js" @@ Dream.from_filesystem "" "tags.bc.js";
  ]
