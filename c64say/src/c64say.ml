let phrase = Cmdliner.Arg.(value & pos 0 string "HELLO" & info [])

let interline =
  let doc = "extra space to insert between lines (in pixels)" in
  Cmdliner.Arg.(value & opt int 0 & info ["interline"] ~doc)

let output =
  let doc = "Output file." in
  Cmdliner.Arg.(value & opt string "isaid.png" & info ["o"; "output"] ~doc)

let translate phrase interline =
  (* make the user figure out the line wrapping for us, for the moment. *)
  let lines = Astring.String.cuts ~sep:"\n" phrase in
  let height = List.length lines in
  let width =
    (* find the longest line *)
    List.fold_left (fun longest s ->
        max (Astring.String.length s) longest) 0 lines
  in
  let image =
    Image.create_grey ~alpha:true (C64chars.w * width)
      ((C64chars.h * height) + (interline * (height - 1)))
  in
  (* try looking up individual chars of the string *)
  List.iteri (fun y line ->
      Astring.String.iteri (fun x c ->
          let pix_list = match C64chars.CharMap.find_opt c C64chars.map with
            | Some l -> l
            | None -> []
          in
          C64chars.paint_pixels image
            ~x_off:(x * C64chars.w)
            ~y_off:((y * C64chars.h) + (y * interline)) pix_list
        ) line) lines;
  image

let say phrase interline output =
  ImageLib_unix.writefile output (translate phrase interline)

let say_t = Cmdliner.Term.(const say $ phrase $ interline $ output)

let info =
  let doc = "make a png representing a phrase in c64 font" in
  Cmdliner.Term.info "c64say" ~doc

let () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (say_t, info)
