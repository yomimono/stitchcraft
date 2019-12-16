(* some PPX-generated code results in warning 39; turn that off *)
[@@@ocaml.warning "-32-39"]

(* a "block" is a region which may contain a stitch. *)
(* the piece is composed of a whole bunch of blocks of constant size
   in a 2d plane. *)

module Block = struct
  type t = int * int (* x, y *) [@@deriving crowbar, eq, yojson]
  let compare (x1, y1) (x2, y2) =
    match Pervasives.compare x1 x2 with
    | 0 -> Pervasives.compare y1 y2
    | n -> n
  let pp = Fmt.(pair int int) [@@toplevel_printer]
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

let pp_stitch fmt = function
  | Full -> Format.fprintf fmt "X"
  | Backslash -> Format.fprintf fmt "\\"
  | Foreslash -> Format.fprintf fmt "/"
  | Backtick -> Format.fprintf fmt "`"
  | Comma -> Format.fprintf fmt ","
  | Reverse_backtick -> Format.fprintf fmt "'"
  | Reverse_comma -> Format.fprintf fmt "."

type thread = DMC.Thread.t
[@@deriving crowbar, eq, yojson]

let pp_thread = Fmt.of_to_string DMC.Thread.to_string

type block = {
  thread : thread;
  stitch : cross_stitch;
}
[@@deriving crowbar, eq, yojson]

let pp_block (fmt : Format.formatter) (block : block) =
  let classify_color (r, g, b) =
    (* TODO: get a better heuristic for this. *)
    if r >= g + b then `Red
    else if b >= r + g then `Blue
    else if g >= r + b then `Green
    else if r > 200 && g > 200 && b > 200 then `White
    else if r <= 50 && g <= 50 && b <= 50 then `Black
    else if r == g && b < 50 then `Yellow
    else if g == b && r < 50 then `Cyan
    else if r == b && g < 50 then `Magenta
    else `White
  in
  let style thread = `Fg (classify_color @@ DMC.Thread.to_rgb thread) in
  let unstyled = Fmt.using (fun b -> b.stitch) pp_stitch in
  let pp = Fmt.styled (style block.thread) unstyled in
  Format.fprintf fmt "%a" pp block

module BlockMap = struct
  include Map.Make(Block)
  let to_yojson m = `List
      (fold (fun key value acc ->
             (`Tuple [Block.to_yojson key;
                      block_to_yojson value])::acc
           ) m [])

(* TODO: we should have some logic somewhere that disallows max_x/max_y
   <=0 . *)
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

let pp_grid fmt g =
  Format.fprintf fmt "%s" (match g with
  | Fourteen -> "14-count"
  | Sixteen -> "16-count"
  | Eighteen -> "18-count")

type substrate =
  { background : RGB.t;
    grid : grid;
    max_x : int; [@generator Crowbar.range 1023](* farthest x coordinate (least is always 0) *)
    max_y : int; [@generator Crowbar.range 1023]
  }
[@@deriving crowbar, eq, yojson]

let pp_substrate fmt {grid; background; _} =
  Format.fprintf fmt "%a aida cloth, color %a" pp_grid grid RGB.pp background

(* TODO: This is a bit ugly; we only define this type to help the ppx_deriving plugins,
   which probably won't need our help if we structure things differently *)
type stitches = block BlockMap.t
let stitches_to_yojson = BlockMap.to_yojson
let stitches_of_yojson = BlockMap.of_yojson
let stitches_to_crowbar = BlockMap.to_crowbar
let equal_stitches = BlockMap.equal equal_block

type state = {
  substrate : substrate;
  stitches : stitches;
} [@@deriving crowbar, eq, yojson]

let pp_state = fun fmt {substrate; stitches} ->
  Format.fprintf fmt "@[background: %a; full size %d x %d@]@." pp_substrate substrate (substrate.max_x + 1) (substrate.max_y + 1);
  (* we'd really like for stitches to be a list of lists, but alas *)
  if substrate.max_x > 80 || substrate.max_y > 100 then
    (* too wide for utop, probably *)
    Format.fprintf fmt "pattern with %d stitches" (BlockMap.cardinal stitches)
  else begin
    (* poky, but we only do it for small substrates, so hopefully not too awful *)
    for y = 0 to substrate.max_y do
      Format.fprintf fmt "@[";
      for x = 0 to substrate.max_x do
        match BlockMap.find_opt (x, y) stitches with
        | Some block -> Format.fprintf fmt "%a" pp_block block
        | None -> Format.fprintf fmt " "
      done;
      Format.fprintf fmt "@]@."
    done
  end

type glyph = {
  stitches : (int * int) list;
  height : int;
  width : int;
} [@@deriving yojson {strict=false}]

module UcharMap = Map.Make(Uchar)

type font = glyph UcharMap.t

type layer = {
  color : RGB.t;
  stitches : (int * int) list;
  height : int;
  width : int;
} [@@deriving yojson]
