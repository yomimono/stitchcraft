
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
