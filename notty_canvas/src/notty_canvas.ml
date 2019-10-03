open Stitchy.Types

type view = {
  x_off : int;
  y_off : int;
  block_display : [ `Symbol | `Solid ];
  zoom : int;
}

let substrate = {
  background = (0, 0, 0);
  grid = Fourteen;
  max_x = 63;
  max_y = 63;
  block_size = 1 (* this is irrelevant for this case, I think *)
}

let printable_symbols = [
  0x23; (* symbol 0x23 *)
  0x25; (* symbol 0x25 *)
  0x2b; (* symbol 0x2b *)
  0x3e; (* symbol 0x3e *)
  0x03a6; (* symbol 0x46 *)
  0x2234; (* symbol 0x5c *)
  0x7b; (* symbol 0x7b *)
  0x221e; (* symbol 0xa5 *)
  0x2666; (* symbol 0xa9 *)
  0xb1; (* symbol 0xb1 *)
  0x21d3; (*symbol 0xdf *)
  0x25ca; (* symbol 0xe0 *)
] |> List.map Uchar.of_int

let or_die (r, g, b) =
  match Stitchy.DMC.Thread.of_rgb (r, g, b) with
  | None -> failwith @@ Format.sprintf "no matching DMC thread for color %d, %d, %d" r g b
  | Some color -> color

let white = or_die (255, 255, 255)
and red = or_die (227, 29, 66)
and orange = or_die (242, 120, 66)
and yellow = or_die (255, 231, 147)
and green = or_die (51, 131, 98)
and blue = or_die (19, 71, 125)
and purple = or_die (128, 58, 107)
let colors = [ white; red; orange; yellow; green; blue; purple ]

let symbol_map =
  let lookups = List.mapi (fun index thread ->
      match List.nth_opt printable_symbols index with
      | None -> (thread, Uchar.of_int 0x2588)
      | Some symbol -> (thread, symbol)
    ) colors in
  List.fold_left (fun map (thread, symbol) ->
      SymbolMap.add (Stitchy.DMC.Thread.to_rgb thread) symbol map
    ) SymbolMap.empty lookups

let stitches : block BlockMap.t =
  let b = BlockMap.empty in
  let cross color = {thread = color;
                 stitch = Full; }
  in
  let add ~x ~y color = BlockMap.add (x, y) (cross color) in
  let b = add ~x:0  ~y:0  white b in
  let b = add ~x:0  ~y:1  blue b in
  let b = add ~x:15 ~y:10 red b in
  let b = add ~x:15 ~y:11 orange b in
  let b = add ~x:15 ~y:12 yellow b in
  let b = add ~x:15 ~y:13 green b in
  let b = add ~x:15 ~y:14 blue b in
  let b = add ~x:15 ~y:15 purple b in
  let b = add ~x:31 ~y:31 white b in
  let b = add ~x:47 ~y:47 white b in
  let b = add ~x:63 ~y:63 white b in
  b

let color_map (a, b, c) =
  let to_6_channels n = n / ((256 / 6) + 1) in
  Notty.A.rgb ~r:(to_6_channels a)
      ~g:(to_6_channels b) ~b:(to_6_channels c)

let colors ~x_off ~y_off ~width ~height stitches =
  let color { thread; _ } = thread in
  (* give an accounting of which colors are represented in the box
   * defined by [(x_off, y_off) ... (x_off + width), (y_off + height)) *)
  let view = BlockMap.submap ~x_off ~y_off ~width ~height stitches in
  BlockMap.bindings view |> List.map (fun (_, block) -> color block) |> List.sort_uniq
    (fun a b -> Stitchy.DMC.Thread.compare a b)

let uchar_of_stitch = function
  | Full -> Uchar.of_int 0x2588
  | Backslash -> Uchar.of_char '\\'
  | Foreslash -> Uchar.of_char '/'
  | Backtick -> Uchar.of_char '`'
  | Comma -> Uchar.of_char ','
  | Reverse_backtick -> Uchar.of_char '?' (* TODO: probably something in uchar works for this *)
  | Reverse_comma -> Uchar.of_char '?'

let symbol_of_block symbols {thread; stitch} =
  match SymbolMap.find_opt (Stitchy.DMC.Thread.to_rgb thread) symbols with
  | Some a -> a
  | None -> uchar_of_stitch stitch

let color_key substrate symbols view colors =
  List.map (fun c ->
      let open Notty.Infix in
      let needle = Stitchy.DMC.Thread.to_rgb c in
      let style = Notty.A.(fg (color_map needle) ++ bg (color_map substrate.background)) in
      let symbol =
        if view.block_display = `Solid then Uchar.of_int 0x2588 else
        match SymbolMap.find_opt needle symbols with
        | Some a -> a
        | None -> Uchar.of_int 0x2588
      in
      Notty.(I.uchar style symbol 1 1) <|> Notty.I.void 1 0 <|>
      Notty.(I.string A.empty @@ Stitchy.DMC.Thread.to_string c)
    ) colors |> Notty.I.vcat

let start_state : state = { substrate; stitches }
let start_view = { x_off = 0; y_off = 0; zoom = 1; block_display = `Symbol }
let key_help _view =
  let open Notty in
  let open Notty.Infix in
  let quit =
    I.string A.(fg lightyellow ++ bg blue) "Q" <|> I.string A.(fg yellow ++ bg black) "uit"
  in
  quit

let show_grid {substrate; stitches} view (width, height) =
  (* figure out how much space we need for grid numbers. *)
  let _max_x, max_y = width + view.x_off, height + view.y_off in
  let label_size n = Notty.(I.width @@ I.string A.empty (Printf.sprintf "%d" n)) in
  let background = Notty.A.(bg @@ color_map substrate.background) in
  let width = width - label_size max_y in
  (* TODO: we assume the x-axis labels will take up only one row,
   * because we're printing them horizontally
   * the right thing to do is pobably to pint them vertically instead,
   * in which case we'll need to adjust this logic. *)
  let height = height - 1 in
  let c x y = match BlockMap.find_opt (x + view.x_off, y + view.y_off) stitches with
    | None -> (* no stitch here *)
      (* TODO: column/row display? *)
      Notty.I.char background ' ' 1 1
    | Some ({thread; _} as block) ->
      let fg_color = color_map @@ Stitchy.DMC.Thread.to_rgb thread in
      Notty.(I.uchar A.(background ++ fg fg_color) (symbol_of_block symbol_map block) 1 1)
  in
  Notty.I.tabulate (min (substrate.max_x + 1) width) (min (substrate.max_y + 1) height) c

let minimap _substrate view (grid_width, grid_height) (minimap_width, minimap_height) =
  let highlighted = Notty.A.(bg green) in
  let not_highlighted = Notty.A.(bg @@ gray 10) in
  let in_view x y =
    if x >= view.x_off && x < (view.x_off + grid_width) &&
       y >= view.y_off && y < (view.y_off + grid_height)
    then highlighted else not_highlighted
  in
  (* TODO: this logic is wrong; we need to map the minimap size onto the grid *)
  (* the minimap is not as big as the grid, so we need to shink/scale
   * what we're showing appropriately *)
  (* so if the minimap is q wide and r tall,
   * and the whole pattern is x wide and y tall,
   * we want to figure out some proportional relationship beween q and x, and r and y, and then figure out what the four corners of the view map to.
   * then we can paint a rectangle of the right size representing the current view, in a field representing the overall view.
   * we need to do somethiing to keep the proportions of the mini-map static though -- 
   * since the color key dynamically grows and shrinks with the view,
   * just using the unused horizontal area will mean the minimap gets stretched
   * and compressed unpredictably, which makes it bad for using to navigate.
   * *)
  Notty.I.tabulate minimap_width minimap_height @@ fun x y ->
      Notty.I.char (in_view x y) ' ' 1 1

let main_view {substrate; stitches} view (width, height) =
  let open Notty.Infix in
  let grid_height = height - 1 in
  let grid_width = width * 2 / 3 in
  let stitch_grid = show_grid {substrate; stitches} view (grid_width, grid_height) in
  let color_key = 
    color_key substrate symbol_map view
     @@ colors ~x_off:view.x_off ~y_off:view.y_off ~width ~height stitches
  in
  let (key_height, key_width) = Notty.I.(height color_key), (width - grid_width)  in
  (* minimap can be the part of the right pane not consumed by the color key *)
  let minimap_height = height - key_height - 1 in
  let minimap_width = key_width in
  (stitch_grid <|>
   (color_key <->
    minimap substrate view (grid_width, grid_height) (minimap_width, minimap_height)
  ))
  <->
  key_help view

let scroll_down substrate view height =
  let next_page = view.y_off + height
  and last_page = substrate.max_y - height
  in
  let best_offset = max (min next_page last_page) 0 in
  { view with y_off = best_offset }

let scroll_right substrate view width =
  let next_page = view.x_off + width
  and last_page = substrate.max_x - width
  in
  let best_offset = max 0 @@ min next_page last_page in
  { view with x_off = best_offset }

let scroll_up view height =
  let prev_page = view.y_off - height
  and first_page = 0
  in
  let best_offset = max prev_page first_page in
  { view with y_off = best_offset }

let scroll_left view width =
  let prev_page = view.x_off - width
  and first_page = 0
  in
  let best_offset = max prev_page first_page in
  { view with x_off = best_offset }

let step state view (width, height) event =
  match event with
  | `Resize _ | `Mouse _ | `Paste _ -> Some (state, view)
  | `Key (key, mods) -> begin
      match key, mods with
      | (`Escape, _) | (`ASCII 'q', _) -> None
      | (`Arrow `Left, _) -> Some (state, scroll_left view width)
      | (`Arrow `Up, _) -> Some (state, scroll_up view height)
      | (`Arrow `Down, _) -> Some (state, scroll_down state.substrate view height)
      | (`Arrow `Right, _) -> Some (state, scroll_right state.substrate view width)
      | _ -> Some (state, view)
    end

let disp () =
  let open Lwt.Infix in
  let term = Notty_lwt.Term.create () in
  Notty_lwt.Term.image term @@ main_view start_state start_view (Notty_lwt.Term.size term) >>= fun () ->
  let rec loop (state : state) (view : view) =
    (Lwt_stream.last_new @@ Notty_lwt.Term.events term) >>= fun event ->
    let size = Notty_lwt.Term.size term in
    match step state view size event with
    | None -> Notty_lwt.Term.release term
    | Some (state, view) ->
      Notty_lwt.Term.image term (main_view state view size) >>= fun () ->
      loop state view
  in
  loop start_state start_view

let () =
  Lwt_main.run @@ disp ()
