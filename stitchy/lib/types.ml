(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-32-39"]

type cross_stitch =
  | Full (* X *) (* full stitch *)
    (* half stitches *)
  | Backslash (* \ *) (* upper left <-> lower right *)
  | Foreslash (* / *) (* lower left <-> upper right *)
    (* quarter stitches *)
  | Backtick (* ` (upper left quadrant) *)
  | Comma (* , (lower left quadrant) *)
  | Reverse_backtick (* mirrored ` (upper right quadrant) *)
  | Reverse_comma (* mirrored , (lower right quadrant) *)
[@@deriving eq, yojson]

type stitch = | Cross of cross_stitch
[@@deriving eq, yojson]

let pp_cross_stitch fmt = function
  | Full -> Format.fprintf fmt "X"
  | Backslash -> Format.fprintf fmt "\\"
  | Foreslash -> Format.fprintf fmt "/"
  | Backtick -> Format.fprintf fmt "`"
  | Comma -> Format.fprintf fmt ","
  | Reverse_backtick -> Format.fprintf fmt "'"
  | Reverse_comma -> Format.fprintf fmt "."

let pp_stitch fmt = function
  | Cross stitch -> pp_cross_stitch fmt stitch

type thread = DMC.Thread.t
[@@deriving eq, yojson]

let pp_thread = Fmt.of_to_string DMC.Thread.to_string

module SymbolMap = Map.Make(DMC.Thread)

type grid = | Fourteen | Sixteen | Eighteen
[@@deriving eq, yojson]

let pp_grid fmt g =
  Format.fprintf fmt "%s" (match g with
  | Fourteen -> "14-count"
  | Sixteen -> "16-count"
  | Eighteen -> "18-count")

type substrate =
  { background : RGB.t;
    grid : grid;
    max_x : int;
    max_y : int;
  }
[@@deriving eq, yojson]

let pp_substrate fmt {grid; background; _} =
  Format.fprintf fmt "%a aida cloth, color %a" pp_grid grid RGB.pp background

type coordinates = int * int [@@deriving yojson]
type segment = coordinates * coordinates [@@deriving yojson]

module Coordinates = struct
  type t = coordinates [@@deriving yojson]
  let compare (x1, y1) (x2, y2) =
    if compare x1 x2 = 0 then compare y1 y2 else compare x1 x2
end
module CoordinateSet = struct
  include Set.Make(Coordinates)
  type coord_list = coordinates list [@@deriving yojson]
  let to_yojson set =
    coord_list_to_yojson @@ elements set
  let of_yojson set =
    match coord_list_of_yojson set with
    | Error e -> Error e
    | Ok coords -> Ok (of_list coords)
end

module Segment = struct
  type t = segment [@@deriving yojson]
  let compare (s1c1, s1c2) (s2c1, s2c2) =
    if Coordinates.compare s1c1 s2c1 = 0
    then Coordinates.compare s1c2 s2c2
    else compare s1c1 s2c1
end
module SegmentSet = struct
  include Set.Make(Segment)
  type segment_list = segment list [@@deriving yojson]
  let to_yojson set =
    segment_list_to_yojson @@ elements set
  let of_yojson set =
    match segment_list_of_yojson set with
    | Error e -> Error e
    | Ok segments -> Ok (of_list segments)
end

type layer = {
  thread : thread; 
  stitch : stitch;
  stitches : CoordinateSet.t;
} [@@deriving eq, yojson]

type layers = layer list [@@deriving eq, yojson]

type backstitch_layer = {
  thread : thread;
  stitches : SegmentSet.t;
} [@@deriving eq, yojson]

type pattern = {
  substrate : substrate;
  layers : layer list; [@default []]
  backstitch_layers : backstitch_layer list; [@default []]
} [@@deriving eq, yojson]

let stitches_at pattern coordinate =
  List.find_all (fun (layer : layer) -> CoordinateSet.(mem coordinate layer.stitches)) pattern.layers |> List.map (fun layer -> (layer.stitch, layer.thread))

let submap ~x_off ~y_off ~width ~height layers =
  let only_stitches_in_submap (layer : layer) =
    let stitches =
      CoordinateSet.filter (fun (x, y)-> x < x_off + width && y < y_off + height) layer.stitches
    in
    {layer with stitches = stitches}
  in
  List.map only_stitches_in_submap layers

let pp_pattern = fun fmt {substrate; layers; backstitch_layers} ->
  Format.fprintf fmt "@[background: %a; full size %d x %d@]@." pp_substrate substrate (substrate.max_x + 1) (substrate.max_y + 1);
  if substrate.max_x > 80 || substrate.max_y > 100 then
    (* too wide for utop, probably *)
    Format.fprintf fmt "pattern with %d layers" (List.length layers)
  else begin
    (* poky, but we only do it for small substrates, so hopefully not too awful *)
    for y = 0 to substrate.max_y do
      Format.fprintf fmt "@[";
      for x = 0 to substrate.max_x do
        match stitches_at {substrate; layers; backstitch_layers } (x, y) with
        | (stitch, _)::_-> Format.fprintf fmt "%a" pp_stitch stitch
        | [] -> Format.fprintf fmt " "
      done;
      Format.fprintf fmt "@]@."
    done
  end

(* lack of dependent types makes us Zalgo-compatible by default *)
type glyph = {
  stitches : CoordinateSet.t;
  height : int;
  width : int;
} [@@deriving yojson {strict=false}]


module UcharMap = Map.Make(Uchar)

type font = glyph UcharMap.t

