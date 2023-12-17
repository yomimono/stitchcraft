let create ~font_size color_to_symbol =
  let paint_symbol (r, g, b) description s n =
    let font_key, symbol = Font.key_and_symbol s in
    let r, g, b = Colors.scale r, Colors.scale g, Colors.scale b in
    let vertical_offset = 1. *. 72. in
    let vertical_step n = (font_size + 4) * n |> float_of_int in
    let swatch_x_offset = 72. -. vertical_step 1 in
    Pdfops.[
      Op_q;
      Op_rg (r, g, b);
      Op_re (swatch_x_offset, vertical_offset +. vertical_step n, vertical_step 1, vertical_step 1);
      Op_f;
      Op_Q;
      Op_q;
      Op_cm
        (Pdftransform.matrix_of_transform
           [Pdftransform.Translate
              (72., vertical_offset +. vertical_step n);
           ]);
      Op_Tf (font_key, (float_of_int font_size));
      Op_BT;
      Op_Tj symbol;
      Font.helvetica_font_op (float_of_int font_size);
      Op_Tj " : ";
      Op_Tj description;
      Op_ET;
      Op_Q;
    ]
  in
  Stitchy.Types.SymbolMap.fold (fun thread symbol (placement, ops) ->
      let description = Stitchy.DMC.Thread.to_string thread in
      let color = Stitchy.DMC.Thread.to_rgb thread in
      let ops = paint_symbol color description symbol placement @ ops in
      (placement + 1, ops)
    ) color_to_symbol (0, [])
