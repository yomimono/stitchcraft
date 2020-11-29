(* test cases: various known font sizes in all known formats *)
(* for formats that aren't byte-aligned, make sure to test non-bit-aligned sizes *)
(* e.g. an 8x8 test, a 16x8 test, a 9x8 test *)

(* readers have to implement `Readfiles.INTERPRETER`, so let's test that *)

open Alcotest

module Glyphmap : Alcotest.TESTABLE with
  type t = Stitchy.Types.glyph list * Uchar.t list list
= struct
  type t = Stitchy.Types.glyph list * Uchar.t list list

  let pp_glyph fmt (glyph : Stitchy.Types.glyph) =
    let block : Stitchy.Types.block = {
      thread = List.hd Stitchy.DMC.Thread.basic;
      stitch = Stitchy.Types.Full;
    } in
    let blockmap = List.fold_left
        (fun m coordinate -> Stitchy.Types.BlockMap.add coordinate block m)
        Stitchy.Types.BlockMap.empty
        glyph.stitches
    in
    let substrate : Stitchy.Types.substrate = {
      grid = Stitchy.Types.Fourteen;
      background = (0, 0, 0);
      max_x = glyph.width - 1;
      max_y = glyph.height - 1;
    } in
    let state : Stitchy.Types.state = { stitches = blockmap; substrate; } in
    Format.fprintf fmt "%a\n%!" Stitchy.Types.pp_state state

  let pp_glyph_list = Fmt.list (pp_glyph)

  let pp_uchars fmt l =
    Format.fprintf fmt "%a" Fmt.(list (list int)) @@ List.map (List.map Uchar.to_int) l

  let pp = Fmt.pair pp_glyph_list pp_uchars

  let glyph_eq (a : Stitchy.Types.glyph) (b : Stitchy.Types.glyph) =
    let open Stitchy.Types in
    a.height = b.height && a.width = b.width &&
    List.length a.stitches = List.length b.stitches &&
    List.for_all2 (fun (a, b) (q, r) -> a = q && b = r) a.stitches b.stitches

  let equal (a_glyphs, a_uchars) (b_glyphs, b_uchars) =
    List.for_all2 glyph_eq a_glyphs b_glyphs &&
    List.for_all2 (fun q r ->
        List.for_all2 (fun a b -> Uchar.equal a b) q r
      ) a_uchars b_uchars

end

let testable_glyphmap = Alcotest.testable Glyphmap.pp Glyphmap.equal

let (testable_err : Fontreader.Otf2stitchfont.error Alcotest.testable) =
  Alcotest.of_pp Fontreader.Otf2stitchfont.pp_error

let zero_length_cstruct =
  let res = Fontreader.Otf2stitchfont.glyphmap_of_buffer (Cstruct.create 0) in
  let exp = Error `No_glyphmap in
  Alcotest.check (result testable_glyphmap pass) "empty buffer gets failure" res exp

let read_glyphmap font =
  let open Rresult.R in
  Fpath.of_string font >>= Bos.OS.File.read >>= fun font ->
  match Fontreader.Otf2stitchfont.glyphmap_of_buffer (Cstruct.of_string font) with
  | Error e -> Error (`Msg (Format.asprintf "%a" Fontreader.Otf2stitchfont.pp_error e))
  | Ok n -> Ok n

let eight_by_eight_ascii font =
  match read_glyphmap font with
  | Error (`Msg f) -> Alcotest.fail f
  | Ok glyphmap ->
    let n_glyphs = List.length @@ fst glyphmap in
    let label = Format.asprintf "%s has %d glyphs" font n_glyphs in
    Alcotest.check bool label true (n_glyphs > 128)

let space_is_empty font =
  match read_glyphmap font with
  | Error (`Msg f) -> Alcotest.fail f
  | Ok (glyphs, uchar_lists) ->
    try
      let lookup_friendly = List.combine glyphs uchar_lists in
      let is_space = Uchar.(equal @@ of_char ' ') in
      let has_space = List.exists is_space in
      let space_glyphs = List.filter (fun (_, uchars) -> has_space uchars) lookup_friendly in
      Alcotest.check int (font ^ ": space char exists") 1 (List.length space_glyphs);
      let space = fst @@ List.hd space_glyphs in
      Alcotest.check int "no stitches for space" 0 (List.length space.stitches)
    with
    Invalid_argument e -> Alcotest.fail e

let full_box_is_full font =
  match read_glyphmap font with
  | Error (`Msg f) -> Alcotest.fail f
  | Ok (glyphs, uchar_lists) ->
    let lookup_friendly = List.combine glyphs uchar_lists in
    let is_box = Uchar.(equal @@ of_int 0x2588) in
    let box_glyphs = List.filter (fun (_, uchars) -> List.exists is_box uchars) lookup_friendly in
    Alcotest.check int (font ^ ": box char exists") 1 (List.length box_glyphs);
    let box = fst @@ List.hd box_glyphs in
    let exp_size = box.width * box.height in
    Alcotest.check int "full box has all stitches populated" exp_size (List.length box.stitches)

let test_font font =
  eight_by_eight_ascii font;
  space_is_empty font;
  full_box_is_full font

let () =
  let eight_by_eight = "./fonts/BmPlus_ToshibaSat_8x8.otb" in
  let eight_by_sixteen = "./fonts/BmPlus_IBM_VGA_8x16.otb" in
  let nine_by_eight = "./fonts/BmPlus_ToshibaSat_9x8.otb" in
  let nine_by_sixteen = "./fonts/BmPlus_IBM_VGA_9x16.otb" in
  let eight_by_fourteen = "./fonts/BmPlus_IBM_VGA_8x14.otb" in

  Alcotest.run "otb fonts" [
    ( "known-good otb fonts", [
          ("8x8", `Quick, fun () -> test_font eight_by_eight);
          ("8x16", `Quick, fun () -> test_font eight_by_sixteen);
          ("9x8", `Quick, fun () -> test_font nine_by_eight);
          ("9x16", `Quick, fun () -> test_font nine_by_sixteen);
          ("8x14", `Quick, fun () -> test_font eight_by_fourteen);
        ])
  ]
