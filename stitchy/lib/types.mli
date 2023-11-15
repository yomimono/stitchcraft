(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-39"]

(* there are two grids of concern:
 * the grid formed by the intersecting warp and weft of the fabric,
 * which is covered by a cross-stitch,
 * and the grid formed by the holes where the warp and weft don't overlap,
 * which is usually of interest for backstitches.
 *)

(* for the "cross-stitch grid", we use integer coordinates
 * starting at (0, 0) on the upper-left corner,
 * increasing toward the lower-right corner.
 *)

(* for the "backstitch grid", we similarly define integer
 * coordinates starting at (0, 0) on the upper-left corner and
 * increasing toward the right and downward.
 * The "backstitch grid" is necessarily 1 larger than the
 * "cross-stitch grid", since it defines the edges of each
 * cross-stitch cell.
 *)

(* Cross-, half-, and quarter-stitches are defined by their location
 * on the cross-stitch grid and their stitch type;
 * backstitches are defined by a start point and an end point
 * on the backstitch grid.
 *)

type coordinates = int * int [@@deriving yojson]
type segment = coordinates * coordinates [@@deriving yojson]
module Coordinates : Map.OrderedType with type t = coordinates
module CoordinateSet : sig
  include Set.S with type elt = coordinates
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
module SegmentSet : sig
  include Set.S with type elt = segment
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

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

type layers = layer list [@@deriving yojson]

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
val submap : x_off:int -> y_off:int -> width:int -> height:int -> pattern -> pattern

val pp_pattern : Format.formatter -> pattern -> unit [@@ocaml.toplevel_printer]

type glyph = {
  stitches : CoordinateSet.t;
  backstitches : SegmentSet.t;
  height : int;
  width : int;
} [@@deriving yojson]

module UcharMap : sig
  include Map.S with type key = Uchar.t
end

type font = glyph UcharMap.t [@@deriving yojson]

type transformation = | Turn | Flip | Nothing [@@deriving yojson]
type transformable_pattern = {
  transformation : transformation; [@default Nothing]
  pattern : pattern;
} [@@deriving yojson]

type border = {
  corner : transformable_pattern;
  side : transformable_pattern option;
  fencepost : transformable_pattern option;
} [@@deriving yojson]
