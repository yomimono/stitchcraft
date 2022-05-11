type obj = {
  id : string;
  obj : string; [@name "object"]
  client_reference_id : string;
} [@@deriving yojson]

type data = {
  obj : obj;  [@name "object"]
} [@@deriving yojson]

type message = {
  api_version : string;
  id : string;
  data : data;
  ty : string; [@name "type"]
} [@@deriving yojson]
