(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-39"]

(* a "block" is a region which may contain a stitch. *)
(* the piece is composed of a whole bunch of blocks of constant size
   in a 2d plane. *)

module Block = struct
  type t = int * int (* x, y *) [@@deriving crowbar, eq, yojson]
  let compare (x1, y1) (x2, y2) =
    match Pervasives.compare x1 x2 with
    | 0 -> Pervasives.compare y1 y2
    | n -> n
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
[@@deriving crowbar, eq, yojson]

type stitch = cross_stitch (* previously, this could be a backstitch;
                              preserve the name, even though we've
                              pulled the backstitch representation for now
                              (since it's not yet clear how to represent it) *)

type thread = DMC.Thread.t
[@@deriving crowbar, eq, yojson]

type block = {
  thread : thread;
  stitch : cross_stitch;
}
[@@deriving crowbar, eq, yojson]

module BlockMap = struct
  include Map.Make(Block)
  let to_yojson m = `List
      (fold (fun key value acc ->
             (`Tuple [Block.to_yojson key;
                      block_to_yojson value])::acc
           ) m [])

  let of_yojson j =
    let convert = function
      | (`Tuple (l : Yojson.Safe.t list)) -> begin
          match l with
          | [] | _::[] | _::_::_::_ -> Error "weird tuple"
          | k::v::[] ->
            (* this ignores unparseable items where it could preserve the error. *)
            let key = Block.of_yojson k in
            let value = block_of_yojson v in
            match (key, value) with
            | Ok key, Ok value -> Ok (key, value)
            | Error s, Ok _ | Ok _, Error s -> Error s
            | Error e, Error s -> Error (e ^ ", and also " ^ s)
        end
      | _ -> Error "Something that wasn't a tuple, when a tuple was expected."
    in
    let l = Yojson.Safe.Util.convert_each convert j in
    List.fold_left (fun acc item ->
        match acc, item with
        | Error e, Error s -> Error (e ^ ", and also " ^ s)
        | Ok _, Error s | Error s, Ok _ -> Error s
        | Ok m, Ok (key, value) -> Ok (add key value m)
      ) (Ok empty) l

  let to_crowbar =
    let key = Crowbar.(map [range 640; range 480] (* not even I would cross-stitch a screenshot bigger than this *) (fun x y -> (x, y))) in
    let value = block_to_crowbar in
    let pair = Crowbar.map [key; value] (fun k v -> k, v) in
    Crowbar.(map [list pair] (fun l ->
        List.fold_left (fun m (k, v) -> add k v m) empty l
      ))

  let submap ~x_off ~y_off ~width ~height map =
    filter (fun (x, y) _ ->
        x >= x_off && x < (x_off + width) &&
        y >= y_off && y < (y_off + height))
      map

end

module SymbolMap = Map.Make(RGB)

type grid = | Fourteen | Sixteen | Eighteen
[@@deriving crowbar, eq, yojson]

type substrate =
  { background : RGB.t;
    grid : grid;
    max_x : int; [@generator Crowbar.range 1023](* farthest x coordinate (least is always 0) *)
    max_y : int; [@generator Crowbar.range 1023]
  }
[@@deriving crowbar, eq, yojson]

(* TODO: This is a bit ugly; we only define this type to help the ppx_deriving plugins,
   which probably won't need our help if we structure things differently *)
type stitches = block BlockMap.t
let stitches_to_yojson = BlockMap.to_yojson
let stitches_of_yojson = BlockMap.of_yojson
let stitches_to_crowbar = BlockMap.to_crowbar
let equal_stitches = BlockMap.equal equal_block

type tool =
  | Block of thread * stitch
  | Eraser

type editor_state =
  { tool : tool; }

type state = {
  substrate : substrate;
  stitches : stitches;
} [@@deriving crowbar, eq, yojson]
