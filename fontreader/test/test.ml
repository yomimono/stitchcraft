(* test cases: various known font sizes in all known formats *)
(* for formats that aren't byte-aligned, make sure to test non-bit-aligned sizes *)
(* e.g. an 8x8 test, a 16x8 test, a 9x8 test *)

(* readers have to implement `Readfiles.INTERPRETER`, so let's test that *)

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

let (testable_err : Fontreader.Otf2stitchfont.error Alcotest.testable) =
  Alcotest.of_pp Fontreader.Otf2stitchfont.pp_error

let zero_length_cstruct =
  let res = Fontreader.Otf2stitchfont.glyphmap_of_buffer true (Cstruct.create 0) in
  let exp = Error `No_glyphmap in
  Alcotest.check (result testable_glyphmap pass) "empty buffer gets failure" res exp

let read_glyphmap font =
  let open Rresult.R in
  Fpath.of_string font >>= Bos.OS.File.read >>= fun font ->
  match Fontreader.Otf2stitchfont.glyphmap_of_buffer true (Cstruct.of_string font) with
  | Error e -> Error (`Msg (Format.asprintf "%a" Fontreader.Otf2stitchfont.pp_error e))
  | Ok n -> Ok n

let eight_by_eight_ascii font =
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

let space_is_empty font =
  let space = fst @@ glyph_in_font font (Uchar.of_char ' ') in
  Format.printf "space char:\n%a\n" Fontreader.Readfiles.print_glyph space;
  Alcotest.check int "no stitches for space" 0 (CoordinateSet.cardinal space.stitches)


let full_box_is_full font =
  let full_box = fst @@ glyph_in_font font (Uchar.of_int 0x2588) in
  let exp_size = full_box.width * full_box.height in
  Alcotest.check int "full box has all stitches populated" exp_size (CoordinateSet.cardinal full_box.stitches)

let box_half_empty font =
  let half_box = fst @@ glyph_in_font font @@ Uchar.of_int 0x2584 in
  let first_row = CoordinateSet.filter (fun (_, y) -> y = 0) half_box.stitches in
  let last_row = CoordinateSet.filter (fun (_, y) -> y = (half_box.height - 1)) half_box.stitches in
  Alcotest.check int "first row of half-box is empty" 0 (CoordinateSet.cardinal first_row);
  Alcotest.check int "last row of half-box is full" half_box.width (CoordinateSet.cardinal last_row)

let test_font font =
  eight_by_eight_ascii font;
  space_is_empty font;
  full_box_is_full font;
  box_half_empty font

let () =
  let eight_by_eight = "./fonts/BmPlus_ToshibaSat_8x8.otb" in
  let eight_by_sixteen = "./fonts/BmPlus_IBM_VGA_8x16.otb" in
  let nine_by_eight = "./fonts/BmPlus_ToshibaSat_9x8.otb" in
  let nine_by_fourteen = "./fonts/BmPlus_IBM_VGA_9x14.otb" in
  let nine_by_sixteen = "./fonts/BmPlus_IBM_VGA_9x16.otb" in
  let eight_by_fourteen = "./fonts/BmPlus_IBM_VGA_8x14.otb" in
  let weird_size = "./fonts/Bm437_IBM_PS-55_re.otb" in

  Alcotest.run "otb fonts" [
    ( "known-good otb fonts", [
          ("8x8", `Quick, fun () -> test_font eight_by_eight);
          ("8x16", `Quick, fun () -> test_font eight_by_sixteen);
          ("9x8", `Quick, fun () -> test_font nine_by_eight);
          ("8x14", `Quick, fun () -> test_font eight_by_fourteen);
          ("9x14", `Quick, fun () -> test_font nine_by_fourteen);
          ("9x16", `Quick, fun () -> test_font nine_by_sixteen);
          ("weird size", `Quick, fun () -> test_font weird_size);
        ])
  ];
