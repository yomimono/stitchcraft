let base = Uri.of_string "https://api.etsy.com"

let openapi_v3 = "/v3/application"

module User = struct
  let get_by_id id = Uri.with_path base (openapi_v3 ^ "/users/" ^ id)

end

module Shop = struct
  let get_by_user_id id = Uri.with_path base (openapi_v3 ^ "/users/" ^ id ^ "/shops")
end
