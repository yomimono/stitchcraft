let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO\\nWORLD" & info [])

let interline =
  let doc = "extra space to insert between lines (in pixels)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output_png =
  let doc = "Output file for png." in
  Cmdliner.Arg.(value & opt string "isaid.png" & info ["o"; "output"] ~doc)

(* this defaults to all pixels being transparent *)
let make_background () = Image.create_grey ~alpha:true C64chars.h C64chars.w

let paint_pixels i ~x_off ~y_off l =
  let alpha = 255 in
  List.iter (fun (x, y) -> Image.write_greya i (x + x_off) (y + y_off) 0 alpha) l

let make_char l : Image.image =
  let i = make_background () in
  paint_pixels i ~x_off:0 ~y_off:0 l;
  i

let translate phrase interline =
  let {C64chars.lines; height; width} = C64chars.get_dimensions phrase interline in
  let image =
    Image.create_grey ~alpha:true width height 
  in
  (* try looking up individual chars of the string *)
  List.iteri (fun y line ->
      Astring.String.iteri (fun x c ->
          let pix_list = match C64chars.CharMap.find_opt c C64chars.map with
            | Some l -> l
            | None -> []
          in
          paint_pixels image
            ~x_off:(x * C64chars.w)
            ~y_off:((y * C64chars.h) + (y * interline)) pix_list
        ) line) lines;
  image

let say phrase interline output =
  ImageLib_unix.writefile output (translate phrase interline)

let say_t = Cmdliner.Term.(const say $ phrase $ interline $ output_png)

let info =
  let doc = "make a png representing a phrase in c64 font" in
  Cmdliner.Term.info "c64say" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (say_t, info)
