(* test cases: various known font sizes in all known formats *)
(* for formats that aren't byte-aligned, make sure to test non-bit-aligned sizes *)
(* e.g. an 8x8 test, a 16x8 test, a 9x8 test *)

module CoordinateSet = Stitchy.Types.CoordinateSet
open Alcotest

module Glyphmap : Alcotest.TESTABLE with
  type t = (Stitchy.Types.glyph * Uchar.t list) list
= struct
  type t = (Stitchy.Types.glyph * Uchar.t list) list

  let pp_uchars fmt l =
    Format.fprintf fmt "%a" Fmt.(list int) @@ List.map Uchar.to_int l

  let pp = Fmt.(list @@ pair Fontreader.Readfiles.print_glyph pp_uchars)

  let glyph_eq (a : Stitchy.Types.glyph) (b : Stitchy.Types.glyph) =
    let open Stitchy.Types in
    a.height = b.height && a.width = b.width &&
    CoordinateSet.equal a.stitches b.stitches

  let ucharl_eq a b =
    List.for_all2 Uchar.equal a b

  let equal a b =
    List.for_all2 (fun (glyph_a, uchars_a) (glyph_b, uchars_b) ->
        glyph_eq glyph_a glyph_b && ucharl_eq uchars_a uchars_b) a b

end

let testable_glyphmap = Alcotest.testable Glyphmap.pp Glyphmap.equal

module Test(Parser : Fontreader.Readfiles.INTERPRETER) = struct
  let (testable_err : Parser.error Alcotest.testable) =
    Alcotest.of_pp Parser.pp_error

  let zero_length_cstruct () =
    let res = Parser.glyphmap_of_buffer true (Cstruct.create 0) in
    match res with
    | Ok _ -> Alcotest.fail "empty buffer gave an OK response"
    | Error _ -> ()

  let read_glyphmap font =
    let open Rresult.R in
    Fpath.of_string font >>= Bos.OS.File.read >>= fun font ->
    match Parser.glyphmap_of_buffer true (Cstruct.of_string font) with
    | Error e -> Error (`Msg (Format.asprintf "%a" Parser.pp_error e))
    | Ok n -> Ok n

  (* minimal test: the font has any glyphs in it at all *)
  let has_some_glyphs font =
    match read_glyphmap font with
    | Error (`Msg f) -> Alcotest.fail f
    | Ok glyphmap ->
      let n_glyphs = List.length glyphmap in
      let minimal_number = 128 in
      let label = Format.asprintf "%s has %d glyphs, which is more than %d" font n_glyphs minimal_number in
      Alcotest.check bool label true (n_glyphs > minimal_number)

  let glyph_in_font font uchar =
    match read_glyphmap font with
    | Error (`Msg f) -> Alcotest.fail f
    | Ok lookup_friendly ->
      let matches = Uchar.equal uchar in
      let glyphs = List.filter (fun (_, uchars) -> List.exists matches uchars) lookup_friendly in

      let msg = Format.asprintf "%s: char 0x%x exists" font (Uchar.to_int uchar) in
      Alcotest.check int msg 1 (List.length glyphs);
      List.hd glyphs

  (* we make the slightly questionable assumption that 0x20 is always going to be blank rather than
   * some kind of marking word boundary. None of the fonts we chose to test with have anything in this glyph,
   * though, so expect no stitches in it. *)
  let space_is_empty font =
    let space = fst @@ glyph_in_font font (Uchar.of_char ' ') in
    Format.printf "space char:\n%a\n" Fontreader.Readfiles.print_glyph space;
    Alcotest.check int "no stitches for space" 0 (CoordinateSet.cardinal space.stitches)

  (* 0x2588 is supposed to be the full box-filling character, so we expect it to be full of stitches
   * in a reasonable font. *)
  let full_box_is_full font =
    let full_box = fst @@ glyph_in_font font (Uchar.of_int 0x2588) in
    let exp_size = full_box.width * full_box.height in
    Alcotest.check int "full box has all stitches populated" exp_size (CoordinateSet.cardinal full_box.stitches)

  (* 0x2584 is supposed to be a half-empty (split horizontally) box. Regardless of how "half" is interpreted,
   * we don't expect to see any stitches in the first row, and we expect the last row to be fully populated.
   * TODO: We also expect for every row to either be completely populated or empty --
   * any partially-populated row is a misaligned translation *)
  let box_half_empty font =
    let half_box = fst @@ glyph_in_font font @@ Uchar.of_int 0x2584 in
    let first_row = CoordinateSet.filter (fun (_, y) -> y = 0) half_box.stitches in
    let last_row = CoordinateSet.filter (fun (_, y) -> y = (half_box.height - 1)) half_box.stitches in
    let full_or_empty_row y =
      let row = CoordinateSet.filter (fun (_, this_y) -> this_y = y) half_box.stitches in
      let n_stitches = CoordinateSet.cardinal row in
      Alcotest.check bool "row is either full or empty" true (n_stitches = 0 || n_stitches = half_box.width)
    in
    Alcotest.check int "first row of half-box is empty" 0 (CoordinateSet.cardinal first_row);
    Alcotest.check int "last row of half-box is full" half_box.width (CoordinateSet.cardinal last_row);
    for row_number = 0 to half_box.height do
      full_or_empty_row row_number
    done

  let test_font font =
    has_some_glyphs font;
    space_is_empty font;
    full_box_is_full font;
    box_half_empty font
end

module Otf = struct
  module Tester = Test(Fontreader.Otf2stitchfont)

  let tests =
    let eight_by_eight = "./fonts/BmPlus_ToshibaSat_8x8.otb" in
    let eight_by_sixteen = "./fonts/BmPlus_IBM_VGA_8x16.otb" in
    let nine_by_eight = "./fonts/BmPlus_ToshibaSat_9x8.otb" in
    let nine_by_fourteen = "./fonts/BmPlus_IBM_VGA_9x14.otb" in
    let nine_by_sixteen = "./fonts/BmPlus_IBM_VGA_9x16.otb" in
    let eight_by_fourteen = "./fonts/BmPlus_IBM_VGA_8x14.otb" in
    let weird_size = "./fonts/Bm437_IBM_PS-55_re.otb" in

    let open Tester in
      "otb parser", [
            ("8x8", `Quick, fun () -> test_font eight_by_eight);
            ("8x16", `Quick, fun () -> test_font eight_by_sixteen);
            ("9x8", `Quick, fun () -> test_font nine_by_eight);
            ("8x14", `Quick, fun () -> test_font eight_by_fourteen);
            ("9x14", `Quick, fun () -> test_font nine_by_fourteen);
            ("9x16", `Quick, fun () -> test_font nine_by_sixteen);
            ("weird size", `Quick, fun () -> test_font weird_size);
            ("empty buffer", `Quick, fun () -> zero_length_cstruct ());
       ]
end

module Yaff = struct
  module Tester = Test(Fontreader.Yaff2stitchfont)

  let tests =
    let open Tester in
    let windows_ega = "./fonts/Terminal_EGA.yaff" in
    ( "yaff parser", [
          ("empty buffer", `Quick, fun () -> zero_length_cstruct ());
          ("height-12 variable width", `Quick, fun () -> test_font windows_ega);

        ])
end

let () =
  Alcotest.run "fonts" [Otf.tests;
                        Yaff.tests;
                       ]
