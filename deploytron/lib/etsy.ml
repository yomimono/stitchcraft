let base = Uri.of_string "https://api.etsy.com"

let openapi_v3 = "/v3/application"

module User = struct
  let get_by_id id = Uri.with_path base (openapi_v3 ^ "/users/" ^ id)

end

module Shop = struct
  type t = {
    shop_id : int;
    listing_active_count : int;
    digital_listing_count : int;
  } [@@deriving yojson { strict = false } ]
  let get_by_user_id id = Uri.with_path base (openapi_v3 ^ "/users/" ^ id ^ "/shops")
end

module Listing = struct
  type t = {
    listing_id : int;
    title : string;
    description : string;
    state : string; (* active | inactive | sold_out | draft | expired *)
    last_modified_timestamp : int; (* epoch seconds *)
    state_timestamp : int; (* last state change. also epoch seconds *)
    quantity : int;
    url : string;
    listing_type : string ; (* physical | download | both *)
    file_data : string; (* "a string describing the files attached to a digital listing" *)
  } [@@deriving yojson { strict = false }]

  type many = {
    results : t list;
  } [@@deriving yojson { strict = false }]

  let get_listings_by_shop shop = Uri.with_path base (openapi_v3 ^ "/shops/" ^ (string_of_int shop) ^ "/listings")

end

module File = struct
  type t = {
    listing_file_id : int;
    listing_id : int;
    rank : int;
    filename : string;
    filetype : string; (* MIME type *)
    create_timestamp : int; (* The unique numeric ID of a file associated with a digital listing., according to thte Etsy documentation, but this seems wrong *)
  } [@@deriving yojson { strict = false }]

  type many = {
    results : t list;
  } [@@deriving yojson { strict = false }]

  let get_files_by_listing ~shop listing = Uri.with_path base @@ openapi_v3 ^ "/shops/" ^ (string_of_int shop) ^ "/listings/" ^ (string_of_int listing) ^ "/files"

  let upload_by_listing_id ~shop listing = Uri.with_path base @@ openapi_v3 ^ "/shops/" ^ (string_of_int shop) ^ "/listings/" ^ (string_of_int listing) ^ "/files"
end

