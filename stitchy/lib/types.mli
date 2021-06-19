(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-39"]

module UcharMap : Map.S with type key = Uchar.t

type coordinates = int * int [@@deriving yojson]
type segment = coordinates * coordinates [@@deriving yojson]
module CoordinateSet : Set.S with type elt = coordinates [@@deriving yojson]
module SegmentSet : Set.S with type elt = segment [@@deriving yojson]

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

val pp_stitch : Format.formatter -> stitch -> unit [@@ocaml.toplevel_printer]

type thread = DMC.Thread.t

val pp_thread : Format.formatter -> thread -> unit [@@ocaml.toplevel_printer]

module SymbolMap : Map.S with type key = thread

(* this is rather unimaginative ;) *)
type grid = | Fourteen | Sixteen | Eighteen

val pp_grid : Format.formatter -> grid -> unit [@@ocaml.toplevel_printer]

type substrate =
  { background : RGB.t;
    grid : grid;
    max_x : int; [@generator Crowbar.range 1023](* farthest x coordinate (least is always 0) *)
    max_y : int; [@generator Crowbar.range 1023]
  }

type layer = {
  thread : thread;
  stitch : stitch;
  stitches : CoordinateSet.t;
} [@@deriving yojson]

type backstitch_layer = {
  thread : thread;
  stitches : SegmentSet.t;
} [@@deriving yojson]

type pattern = {
  substrate : substrate;
  layers : layer list; [@default []]
  backstitch_layers : backstitch_layer list; [@default []]
} [@@deriving yojson]

val stitches_at: pattern -> (int * int) -> (stitch * thread) list
val submap : x_off:int -> y_off:int -> width:int -> height:int -> layer list -> layer list

val pp_pattern : Format.formatter -> pattern -> unit [@@ocaml.toplevel_printer]

type glyph = {
  stitches : CoordinateSet.t;
  height : int;
  width : int;
} [@@deriving yojson]

type font = glyph UcharMap.t
