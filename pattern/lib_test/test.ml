let coords_equal (a, b, c, d) (one, two, three, four) =
  a = one && b = two && c = three && d = four

let pp_coords fmt (page_grid_x, page_grid_y, pagewise_x, pagewise_y) =
  Format.fprintf fmt "pixel (%d, %d) on page (%d, %d)" pagewise_x pagewise_y
    page_grid_x page_grid_y

let coords = Alcotest.testable pp_coords coords_equal
let check_coords = Alcotest.check coords

let pixel_size = 10
let paper = Pdfpaper.usletter
let get_coords = Pattern.Output_pdf.page_of_stitch ~pixel_size ~paper
let get_big_coords = Pattern.Output_pdf.page_of_stitch ~pixel_size:20 ~paper

let origin () = check_coords "origin" (0, 0, 0, 0) (get_coords (0, 0))
let first_page () =
  check_coords "first page" (0, 0, 0, 1) (get_coords (0, 1));
  check_coords "end of first page" (0, 0, 49, 0) (get_coords (49, 0));
  check_coords "end of first page" (0, 0, 49, 63) (get_coords (49, 63));
  check_coords "end of first page w/big pixels" (0, 0, 24, 31) (get_big_coords (24, 31))

let second_page () =
  check_coords "second page" (1, 0, 0, 0) (get_coords (50, 0));
  check_coords "second page" (1, 0, 0, 63) (get_coords (50, 63));
  check_coords "second page w/big pixels" (1, 0, 0, 31) (get_big_coords (25, 31));
  check_coords "end of second page" (1, 0, 49, 0) (get_coords (99, 0));
  check_coords "end of second page" (1, 0, 49, 63) (get_coords (99, 63))

let second_row () = check_coords "second page" (0, 1, 0, 6) (get_coords (0, 74))

let () = Alcotest.run "pattern lib functions" [
    ("the grid", [
        Alcotest.test_case "origin" `Quick origin;
        Alcotest.test_case "first_page" `Quick first_page;
        Alcotest.test_case "second_page" `Quick second_page;
        Alcotest.test_case "second_row" `Quick second_row;
      ]
    );
  ]

