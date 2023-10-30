open Stitchy.Types

type direction = | Up | Down

type traverse = {
  n : int;
  contents : Fpath.t list;
  direction : direction;
}

let reset_view view = Controls.({
  view with x_off = 0; y_off = 0;
})

let filesystem_pane {n; contents; _} (_width, _height) =
  List.fold_left (fun (i, acc) filename ->
      if i < n then (i + 1, acc)
      else if i = n then
        (i + 1, Notty.I.(vcat [acc; string Notty.A.(bg yellow ++ st bold) @@ Fpath.basename filename]))
      else
        (i + 1, Notty.I.(vcat [acc; string Notty.A.empty @@ Fpath.basename filename]))
    ) (0, Notty.I.empty) contents |> snd

let success (width, height) s = 
    Notty.I.hsnap width @@ Notty.I.vsnap height @@
    Notty.I.string Notty.A.(bg green ++ fg white ++ st bold) s

let failure (width, height) s =
    Notty.I.hsnap width @@ Notty.I.vsnap height @@
    Notty.I.string Notty.A.(bg red ++ fg white ++ st bold) s

let symbol_map colors =
  let lookups = List.mapi (fun index thread ->
      match List.nth_opt Stitchy.Symbol.printable_symbols index with
      | None -> (thread, Uchar.of_int 0x2588)
      | Some symbol -> (thread, symbol.uchar)
    ) colors in
  List.fold_left (fun map (thread, symbol) ->
      SymbolMap.add thread symbol map
    ) SymbolMap.empty lookups

let invert (r, g, b) =
  (255 - r, 255 - g, 255 - b)

let color_map (r, g, b) =
  Notty.A.rgb_888 ~r ~g ~b

let colors ~x_off ~y_off ~width ~height pattern =
  (* give an accounting of which colors are represented in the box
   * defined by [(x_off, y_off) ... (x_off + width), (y_off + height)) *)
  let view : layer list = Stitchy.Types.submap ~x_off ~y_off ~width ~height pattern.layers in
  List.filter_map (fun (layer : layer) ->
      match CoordinateSet.is_empty layer.stitches with
      | true -> None
      | false -> Some layer.thread) view

let uchar_of_cross_stitch = function
  | Full -> Uchar.of_int 0x2588
  | Backslash -> Uchar.of_char '\\'
  | Foreslash -> Uchar.of_char '/'
  | Backtick -> Uchar.of_char '`'
  | Comma -> Uchar.of_char ','
  | Reverse_backtick -> Uchar.of_char '?' (* TODO: probably something in uchar works for this *)
  | Reverse_comma -> Uchar.of_char '?'

let uchar_of_stitch = function
  | Cross n -> uchar_of_cross_stitch n

let symbol_of_thread symbols (stitch, thread) =
  match SymbolMap.find_opt thread symbols with
  | Some a -> a
  | None -> uchar_of_stitch stitch

let label_size n = Notty.(I.width @@ I.strf "%d" n)

let label_y_axis ~width ~start_y ~max_y style =
  let rec next_line l n = if n > max_y then List.rev l else
      let i = Notty.I.hsnap ~align:(`Right) width @@
        Notty.I.strf ~attr:style "%d" n in
      next_line (i::l) (n+1)
  in
  Notty.I.vcat @@ next_line [] start_y

let label_x_axis ~height ~start_x ~max_x style =
  let verticalize s =
    Astring.String.fold_left (fun l c -> Notty.I.char style c 1 1 :: l) [] s
    |> Notty.I.vcat
  in
  let rec next_line l n = if n > max_x then List.rev l else
      let l = (Notty.I.vsnap ~align:(`Bottom) height @@
               verticalize (string_of_int n)) :: l in
      next_line l (n+1)
  in
  Notty.I.hcat @@ next_line [] start_x

(* given a width and height for the left pane,
   figure out the dimensions appropriate for the x and y axis labels
   as well as the stitch grid representation itself. *)
let subdivide_left_pane ~left_pane_width ~left_pane_height substrate =
  (* this was previously scaled for the largest number in the view,
     but it's disconcerting when the grid moves,
     so scale it for the largest number that will appear. *)
  let y_axis_label_width = label_size substrate.max_y
  and x_axis_label_height = label_size substrate.max_x
  in
  let stitch_grid_width = left_pane_width - y_axis_label_width
  and stitch_grid_height = left_pane_height - x_axis_label_height
  in
  { Controls.empty_corner = { width = y_axis_label_width;
                     height = x_axis_label_height;
                   };
    stitch_grid = { width = stitch_grid_width;
                    height = stitch_grid_height;
                  }
  }

let left_pane substrate (width, height) =
  let height = height - 1 in
  let width = width * 2 / 3 in
  subdivide_left_pane ~left_pane_width:width ~left_pane_height:height substrate

let show_left_pane {substrate; layers; backstitch_layers} symbol_map state left_pane =
  let open Notty.Infix in
  let background = Notty.A.(bg @@ color_map substrate.background) in
  let c x y =
    let stitches_here =
      Stitchy.Types.stitches_at {substrate; layers; backstitch_layers}
        (x + state.Controls.view.x_off, y + state.view.y_off)
    in
    match stitches_here, state.selection with
    | [], None -> Notty.I.char background ' ' 1 1
    | [], Some selection ->
      (* get the bounds for the selection *)
      let {Controls.start_cell; end_cell} = Controls.normalize_selection selection in
      let min_x, min_y = start_cell and max_x, max_y = end_cell in
      (* if (x, y) is in bounds, set the background color to the inverse *)
      if min_x <= x && x <= max_x && min_y <= y && y <= max_y then
        Notty.I.char Notty.A.(bg @@ color_map @@ invert substrate.background) ' ' 1 1
      (* for (x, y) out of bounds, leave the empty background *)
      else Notty.I.char background ' ' 1 1
    | ((stitch, thread)::_), _ ->
      let symbol = match state.Controls.view.block_display with
        | `Symbol -> symbol_of_thread symbol_map (stitch, thread)
        | `Solid -> uchar_of_stitch stitch
      in
      let fg_color = color_map @@ Stitchy.DMC.Thread.to_rgb thread in
      Notty.(I.uchar A.(background ++ fg fg_color) symbol 1 1)
  in
  let view = state.view in
  let max_x = min substrate.max_x Controls.((view.x_off + left_pane.stitch_grid.width) - 1)
  and max_y = min substrate.max_y Controls.((view.y_off + left_pane.stitch_grid.height) - 1)
  in
  let label_axes i =
    Notty.I.void left_pane.empty_corner.width left_pane.empty_corner.height
    <-> label_y_axis ~width:left_pane.empty_corner.width ~start_y:view.y_off ~max_y Notty.A.empty
    <|> (label_x_axis ~height:left_pane.empty_corner.height ~start_x:view.x_off ~max_x Notty.A.empty <-> i)
  in
  label_axes @@
  Notty.I.tabulate left_pane.stitch_grid.width left_pane.stitch_grid.height c

let key_help view =
  let open Notty in
  let open Notty.Infix in
  let highlight = A.(fg lightyellow ++ bg blue)
  and lowlight = A.(fg yellow ++ bg black)
  in
  let symbol_text = match view.Controls.block_display with
    | `Solid -> "symbol view"
    | `Symbol -> "solid view"
  in
  let sp = I.void 1 1 in
  let nav_text = I.string highlight "←↑→↓" <|> I.string lowlight " to scroll"
  and shift_text = I.string highlight "Shift + ←↑→↓" <|> I.string lowlight " to page"
  and quit = I.string highlight "Q" <|> I.string lowlight "uit"
  and symbol = I.string highlight "Tab" <|> sp <|> I.string lowlight symbol_text
  in
  quit <|> sp <|> symbol <|> sp <|> nav_text <|> sp <|> shift_text

let save_dialog filename_entry =
  let open Notty in
  Infix.(<->) (I.string A.empty "save to filename:")
  (I.hcat @@ List.rev_map (fun u -> I.uchar A.empty u 1 1) filename_entry)

let main_view traverse pattern state (width, height) =
  let open Notty.Infix in
  let aux {substrate; layers; backstitch_layers} =
    let symbol_map = symbol_map @@ List.map (fun (layer : Stitchy.Types.layer) -> layer.thread) layers in
    let left_pane = left_pane substrate (width, height) in
    let stitch_grid = show_left_pane {substrate; layers; backstitch_layers;} symbol_map state left_pane in
    (* to separate into two panes vertically, divide then remove 2 more rows for spacer and key help *)
    let fs = (Notty.I.vsnap ~align:`Top height @@ filesystem_pane traverse (width, height))
    in
    (stitch_grid <|> Notty.I.void 1 1 <|> fs)
    <->
    key_help state.view
  in
  match state.Controls.mode with
  | Save f -> save_dialog f
  | Browse -> aux pattern
  | Preview ->
    (* in the preview mode, see how it looks tiled *)
    aux Stitchy.Operations.(vrepeat (hrepeat pattern 3) 3)

let crop source_pattern state =
  match state.Controls.selection with
  | None -> Error "crop with no selection"
  | Some s ->
    let {Controls.start_cell; end_cell} = Controls.normalize_selection s in
    let min_x, min_y = start_cell and max_x, max_y = end_cell in
    match source_pattern.backstitch_layers with
    | _::l -> Error (Format.asprintf "%d layers of backstitch present" ((+) 1 @@ List.length l))
    | [] ->
      (* because `transform` wants a pattern, we somewhat counterintuitively
       * transform all the stitches first, then ask for the subview on
       * the selected area *)
      let shifted =
        Stitchy.Operations.transform_all_stitches
          ~f:(fun (x, y) -> x - min_x, y - min_y)
          source_pattern
      in
      let layers = Stitchy.Types.submap
          ~x_off:0 ~y_off:0
          ~width:(max_x - min_x + 1) ~height:(max_y - min_y + 1)
      shifted.layers in
      let substrate = { source_pattern.substrate with max_x = max_x - min_x;
                                                      max_y = max_y - min_y;
                      } in
      Ok { layers;
        backstitch_layers = [];
        substrate;
      }

let handle_mouse state left_pane button =
  let offset_click (x, y) =
    let empty_corner = left_pane.Controls.empty_corner in
    if x < empty_corner.width || y < empty_corner.height then
      `None
    else
      let selection_x = x - empty_corner.width
      and selection_y = y - empty_corner.height
      in
      let selection = (selection_x, selection_y) in
      `Grid selection
  in
  match button with
  | ((`Press (`Left)), (x, y), _) ->
    begin
    match offset_click (x, y) with
    | `None -> `None, state
    | `Grid selection ->
      `None, {state with Controls.selection = Some {start_cell = selection; end_cell = selection}}
  end
  | (`Release, (x, y), _) | (`Drag, (x, y), _) -> begin
    match state.selection, offset_click (x, y) with
    | None, _ | _, `None -> `None, state
    | (Some {start_cell; _}), (`Grid selection) ->
      `None, {state with selection = Some {start_cell; end_cell = selection}}
  end
  | _ -> `None, state

let step state pattern (width, height) event =
  let left_pane = left_pane pattern.substrate (width, height) in
  match event with
  | `Resize _ | `Paste _ -> `None, state
  | `Mouse button -> handle_mouse state left_pane button
  | `Key (key, mods) -> begin
      match state.mode, key, mods with
      | Save f, `Enter, _ -> (`Save f), state
      | Save f, k, _ -> Controls.handle_typing state f k
      | Preview, `ASCII 's', _ ->
        let filename_entry = [] in
        (`Typing filename_entry, { state with mode = Controls.(Save filename_entry)})
      (* application control *)
      | _, `Escape, _ | _, `ASCII 'q', _ -> `Quit, state
      (* view control *)
      | _, `Arrow dir, l when List.mem `Shift l -> `None, {state with view = Controls.page pattern.substrate state.view left_pane dir}
      | _, `Arrow dir, _ -> `None, {state with view = Controls.scroll pattern.substrate state.view dir }
      | _, `Tab, _ -> `None, {state with view = Controls.switch_view state.view}
      (* pattern control *)
      | Browse,  `ASCII 'n', _ -> `Next, {state with view = reset_view state.view; selection = None}
      | Browse, `ASCII 'p', _ -> `Prev, {state with view = reset_view state.view; selection = None}
      | Browse, `ASCII 'c', _ -> `Crop, {state with mode = Preview}
      (* default *)
      | _, _, _ -> (`None, state)
    end
