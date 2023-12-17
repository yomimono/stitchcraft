let t1_font name =
  Pdf.(Dictionary
         [("/Type", Name "/Font");
          ("/Subtype", Name "/Type1");
          ("/BaseFont", Name name);])

let symbol_font = t1_font "/Symbol"
and zapf_font = t1_font "/ZapfDingbats"
and helvetica_font = t1_font "/Helvetica"

let symbol_key = "/F0"
and zapf_key = "/F1"
and helvetica_key = "/F2"

let key_and_symbol s = match s.Stitchy.Symbol.pdf with
  | `Zapf symbol -> zapf_key, symbol
  | `Symbol symbol -> symbol_key, symbol

let font_resources =
   Pdf.(Dictionary [
       ("/Font", Pdf.Dictionary [(symbol_key, symbol_font);
                                 (zapf_key, zapf_font);
                                 (helvetica_key, helvetica_font)]);
     ])

let helvetica_font_op text_size =
  Pdfops.(Op_Tf (helvetica_key, text_size))

