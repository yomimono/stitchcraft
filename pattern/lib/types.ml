type borders = {
  max_x : float;
  max_y : float;
  min_x : float;
  min_y : float;
}

type doc = {
  symbols : Stitchy.Symbol.t Stitchy.Types.SymbolMap.t;
  pixel_size : int;
  paper_size : Pdfpaper.t;
  fat_line_interval : int;
}

type page = {
  x_range : int * int;
  y_range : int * int;
  page_number : int;
}

type representation =
  | Symbol of Stitchy.RGB.t * Stitchy.Symbol.t
  | Line of Stitchy.RGB.t * Stitchy.Types.back_stitch

