(* here's what imagemagick has to say about PPM:

   PNM is a family of formats supporting portable bitmaps (PBM) , graymaps (PGM), and pixmaps (PPM). There is no file format associated with pnm itself. If PNM is used as the output format specifier, then ImageMagick automagically selects the most appropriate format to represent the image. The default is to write the binary version of the formats. Use -compress none to write the ASCII version of the formats.

*)

(* in that case, the header is still written with ascii, but the data is binary, like this header:
00000000: 5036 0a32 3720 3434 0a31 350a 0008 0400  P6.27 44.15.....
00000010: 0804 0008 0400 0804 0008 0400 0804 0008  ................
00000020: 0400 0804 0008 0400 0804 0008 0400 0804  ................

   this goes beyond the examples offered in netpbm's own documentation, where max_color_size 255 and 65535 alone
   are mentioned - the example generated above is an image with fewer colors, and I guess they
   can all get squished.

   if we ask for -compress none, we get a different result in the header as well as the data:

00000000: 5033 0a32 3720 3434 0a32 3535 0a30 2031  P3.27 44.255.0 1
00000010: 3336 2036 3820 3020 3133 3620 3638 2030  36 68 0 136 68 0
00000020: 2031 3336 2036 3820 3020 3133 3620 3638   136 68 0 136 68
*)

type header = {
  width : int;
  height : int;
  max_color_value : int;
}

let is_digit c =
  (Char.code c >= 0x30 && Char.code c <= 0x39)

let decimal_number =
  let open Angstrom in
  take_while is_digit >>| int_of_string

let skip_nondigits = Angstrom.skip_while (fun c -> not @@ is_digit c)
and take_digits = Angstrom.take_while (fun c -> is_digit c)

module RGB = Stitchy.RGB
module RGBMap = Map.Make(RGB)

let pixel =
  let open Angstrom in
  skip_nondigits *>
  decimal_number >>= fun r ->
  skip_nondigits *>
  decimal_number >>= fun g ->
  skip_nondigits *>
  decimal_number >>= fun b ->
  return (r, g, b)

let maybe_add_pixel ~ignore ~x ~y pixel m =
  match List.mem pixel ignore with
  | true -> m
  | false ->
    RGBMap.update pixel (function
        | None -> Some [(x, y)]
        | Some l -> Some ((x, y)::l)
      ) m

let header =
  let open Angstrom in
  let strint = Astring.String.to_int in
  Angstrom.string "P3" *>
  skip_nondigits *>
  take_digits >>= fun width_str ->
  skip_nondigits *>
  take_digits >>= fun height_str ->
  skip_nondigits *>
  take_digits >>= fun max_color_value_str ->
  skip_nondigits *>
  match strint width_str, strint height_str, strint max_color_value_str with
  | Some width, Some height, Some max_color_value -> return @@ Ok { width; height; max_color_value }
  | _, _, _ -> return (Error (`Msg "netppm header parsing failed"))

let image ~ignore m width height =
  let open Angstrom in
  Angstrom.count (width * height) pixel >>= fun pixels ->
  let _, m = List.fold_left (fun (n, m) pixel ->
      let y = n / width
      and x = n mod width
      in
      (n + 1), (maybe_add_pixel ~ignore ~x ~y pixel m)
    ) (0, m) pixels
  in
  return m

(* `ignore` is a list of colors to ignore when seen.
 * usually this is because that's the color of the background,
 * so you don't need a stitch there *)
let read_ppm ~ignore ~debug =
  let open Angstrom in
  header >>= function
  | Error (`Msg e) -> failwith e
  | Ok h ->
    if debug then Printf.printf "reading a %d x %d image\n%!" h.width h.height;
    image ~ignore RGBMap.empty h.width h.height >>= fun layers -> return (h, layers)

let patternfy ~grid ~algo ~stitch ~ignore ~background debug image =
  (* image is a good ol' string fulla bytes *)
  match Angstrom.parse_string ~consume:Prefix (read_ppm ~debug ~ignore) image with
  | Error s -> Error (`Msg s)
  | Ok (header, color_to_coordinates) ->
    let layers = Color_matcher.translate ~debug ~algo ~stitch color_to_coordinates in
    let max_x = header.width - 1
    and max_y = header.height - 1
    in
    let (substrate : Stitchy.Types.substrate) = { background; grid; max_x; max_y } in
    let (pattern : Stitchy.Types.pattern) = {substrate; layers; backstitch_layers = []} in
    Ok pattern
