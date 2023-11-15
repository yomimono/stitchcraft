open Lwt.Infix

let sig_header = "Stripe-Signature"

let verify_sig ~earliest_valid ~latest_valid ~t ~v1 body =
  Dream.debug (fun log -> log "verifying a signature with timestamp %s" t);
  let to_sign = Cstruct.of_string (t ^ "." ^ body) in
  let body_signature = Mirage_crypto.Hash.SHA256.hmac ~key:(Cstruct.of_string Secrets.webhook_test_sk) to_sign in
  try
    let cs_v1 = Cstruct.of_string @@ Astring.String.trim @@ Hex.to_string (`Hex v1) in
    let signature_checks_out = Cstruct.(equal cs_v1 body_signature) in
    if not signature_checks_out then Dream.debug
        (fun log -> log "transmitted signature %a and computed signature %a don't match" Cstruct.hexdump_pp cs_v1 Cstruct.hexdump_pp body_signature);
    signature_checks_out && (int_of_string t >= earliest_valid) && (int_of_string t <= latest_valid)
  with _ ->
    Dream.debug (fun log -> log "timestamp parsing failed for signature validation");
    false

let check_signature request =
  match Dream.headers request sig_header with
  | [] | _::_::_ ->
    Dream.debug (fun l -> l "sig header not present in something that needs a signature");
    Lwt.return false
  | purported_signature::[] ->
    match Astring.String.fields ~is_sep:(Char.equal ',') purported_signature with
    | [] ->
      Dream.debug (fun l -> l "sig header didn't have parseable comma-separated fields");
      Lwt.return false
    | l ->
      let kv = List.map (Astring.String.span ~sat:(fun c -> not @@ Char.equal '=' c)) l in
      let get_all needle =
        List.filter_map (fun (k, v) -> if String.equal k needle then
                            let no_equals = Astring.String.filter (fun c -> not @@ Char.equal '=' c) v in
                            Some no_equals
                          else None)
      in
      let timestamps = get_all "t" kv in
      let v1_sigs = get_all "v1" kv in
      match timestamps, v1_sigs with
      | t::[], v1::[] ->
        (* TODO: check timestamp against current time; we ain't need to get replay-attacked *)
        (* [t] is seconds-since-epoch in decimal notation AFAICT *)
        (* I don't want to plumb this in until we're working with Mirage properly *)
        Dream.body request >|= verify_sig ~earliest_valid:0 ~latest_valid:Int.max_int ~t ~v1
      | _ ->
        Dream.debug (fun log -> log "expected number of timestamps and v1 signatures not found");
        Lwt.return false

let try_fulfill request =
  fun (module Db : Caqti_lwt.CONNECTION) ->
  check_signature request >>= function
  | false ->
    Dream.debug (fun f -> f "signature verification failed");
    Dream.respond ~code:400 ""
  | true ->
    Dream.debug (fun f -> f "signature verified for fulfillment request");
    (* TODO: OK, now do something with it *)
    Dream.body request >>= fun body ->
    try
      let json = Yojson.Safe.from_string body in
      Dream.debug (fun f -> f "json parsed from body");
      match Stripe.message_of_yojson json with
      | Error s -> Dream.debug (fun f -> f "message parsing failed: %s" s);
        Dream.respond ~code:400 ""
      | Ok stripe_msg ->
        let open Stripe in
        (* TODO: I think what we care about here is really the "payment intent",
         * which I think we make the ID for?  Nope, we POST and it gives us one, which is a little :/ 
         * *)
        Dream.debug (fun f -> f "client reference id %s got message type %s" stripe_msg.data.obj.client_reference_id stripe_msg.ty);
        Dream.respond ~code:501 ""
    with
    | _ -> Dream.respond ~code:400 ""

let () =
  Dream.initialize_log ~level:`Debug ();
  Dream.(run @@ logger @@ memory_sessions @@ sql_pool "postgresql://stitchcraft:lolbutts@localhost:5432" @@ router [
      Dream.post "/fulfill" (fun request -> Dream.sql request @@ try_fulfill request)

  ])
