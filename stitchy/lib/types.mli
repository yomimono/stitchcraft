(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-39"]

module Block : sig
  type t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
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

type stitch = cross_stitch (* previously, this could be a backstitch;
                              preserve the name, even though we've
                              pulled the backstitch representation for now
                              (since it's not yet clear how to represent it) *)

val pp_stitch : Format.formatter -> stitch -> unit [@@ocaml.toplevel_printer]

type thread = DMC.Thread.t

val pp_thread : Format.formatter -> thread -> unit [@@ocaml.toplevel_printer]

type block = {
  thread : thread;
  stitch : cross_stitch;
}

val pp_block : Format.formatter -> block -> unit [@@ocaml.toplevel_printer]

module BlockMap : sig
  include Map.S with type key = int * int
  val to_yojson : block t -> [> `List of [> `Tuple of Yojson.Safe.t list ] list ]

  val of_yojson : Yojson.Safe.t -> (block t, string) result

  val to_crowbar : block t Crowbar.gen

  val submap : x_off:int -> y_off:int -> width:int -> height:int -> 'a t -> 'a t

end

module SymbolMap : Map.S with type key = RGB.t

type grid = | Fourteen | Sixteen | Eighteen

val pp_grid : Format.formatter -> grid -> unit [@@ocaml.toplevel_printer]

type substrate =
  { background : RGB.t;
    grid : grid;
    max_x : int; [@generator Crowbar.range 1023](* farthest x coordinate (least is always 0) *)
    max_y : int; [@generator Crowbar.range 1023]
  }

(* TODO: This is a bit ugly; we only define this type to help the ppx_deriving plugins,
   which probably won't need our help if we structure things differently *)
type stitches = block BlockMap.t
val stitches_to_yojson : block BlockMap.t -> Yojson.Safe.t
val stitches_of_yojson : Yojson.Safe.t -> (block BlockMap.t, string) result
val stitches_to_crowbar : block BlockMap.t Crowbar.gen
val equal_stitches : block BlockMap.t -> block BlockMap.t -> bool

type state = {
  substrate : substrate;
  stitches : stitches;
} [@@deriving yojson]

val pp_state : Format.formatter -> state -> unit [@@ocaml.toplevel_printer]

type glyph = {
  stitches : (int * int) list;
  height : int;
  width : int;
} [@@deriving yojson]

module UcharMap : Map.S

type font = glyph UcharMap.t

type layer = {
  color : RGB.t;
  stitches : (int * int) list;
  height : int;
  width : int;
} [@@deriving yojson]
