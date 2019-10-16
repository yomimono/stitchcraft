open Stitchy.Types

let symbol_map colors =
  let lookups = List.mapi (fun index thread ->
      match List.nth_opt Stitchy.Symbol.printable_symbols index with
      | None -> (thread, Uchar.of_int 0x2588)
      | Some symbol -> (thread, symbol.uchar)
    ) colors in
  List.fold_left (fun map (thread, symbol) ->
      SymbolMap.add (Stitchy.DMC.Thread.to_rgb thread) symbol map
    ) SymbolMap.empty lookups

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
        if view.Controls.block_display = `Solid then Uchar.of_int 0x2588 else
        match SymbolMap.find_opt needle symbols with
        | Some a -> a
        | None -> Uchar.of_int 0x2588
      in
      Notty.(I.uchar style symbol 1 1) <|> Notty.I.void 1 0 <|>
      Notty.(I.string style @@ Stitchy.DMC.Thread.to_string c)
    ) colors |> Notty.I.vcat

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

let show_left_pane {substrate; stitches} symbol_map view left_pane =
  let open Notty.Infix in
  let background = Notty.A.(bg @@ color_map substrate.background) in
  let c x y = match BlockMap.find_opt (x + view.Controls.x_off, y + view.y_off) stitches with
    | None -> (* no stitch here *)
      (* TODO: column/row display? *)
      Notty.I.char background ' ' 1 1
    | Some ({thread; _} as block) ->
      let symbol = match view.block_display with
        | `Symbol -> symbol_of_block symbol_map block
        | `Solid -> Uchar.of_int 0x2588
      in
      let fg_color = color_map @@ Stitchy.DMC.Thread.to_rgb thread in
      Notty.(I.uchar A.(background ++ fg fg_color) symbol 1 1)
  in
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
    | `Solid -> "ymbol view"
    | `Symbol -> "olid view"
  in
  let nav_text = I.string highlight "←↑→↓" <|> I.string lowlight " to scroll"
  and shift_text = I.string highlight "Shift + ←↑→↓" <|> I.string lowlight " to page"
  and quit = I.string highlight "Q" <|> I.string lowlight "uit"
  and symbol = I.string highlight "S" <|> I.string lowlight symbol_text in
  quit <|> I.void 1 1 <|> symbol <|> I.void 1 1 <|> nav_text <|> I.void 1 1 <|> shift_text

let main_view {substrate; stitches} view (width, height) =
  let open Notty.Infix in
  let just_stitches = Stitchy.Types.BlockMap.bindings stitches |> List.map snd |> List.sort_uniq (fun block1 block2 -> Stitchy.DMC.Thread.compare block1.thread block2.thread) in
  let symbol_map = symbol_map (List.map (fun b -> b.thread) just_stitches) in
  let left_pane = left_pane substrate (width, height) in
  let stitch_grid = show_left_pane {substrate; stitches} symbol_map view left_pane in
  let colors = colors ~x_off:view.x_off ~y_off:view.y_off
      ~width:left_pane.stitch_grid.width ~height:left_pane.stitch_grid.height 
      stitches
  in
  let color_key = color_key substrate symbol_map view colors in
  (stitch_grid <|> color_key)
  <->
  key_help view

let step state view (width, height) event =
  (* TODO: scrolling should, by default, be one pixel; big steps should be done with modifiers *)
  let left_pane = left_pane state.substrate (width, height) in
  match event with
  | `Resize _ | `Mouse _ | `Paste _ -> Some (state, view)
  | `Key (key, mods) -> begin
      match key, mods with
      | (`Escape, _) | (`ASCII 'q', _) -> None
      | (`Arrow dir, l) when List.mem `Shift l -> Some (state, Controls.scroll_page state.substrate view left_pane dir)
      | (`Arrow dir, _) -> Some (state, Controls.scroll_one state.substrate view dir)
      | (`ASCII 's', _) -> Some (state, Controls.switch_view view)
      | _ -> Some (state, view)
    end
