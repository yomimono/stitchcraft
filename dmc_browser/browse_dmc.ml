open Lwt.Infix

let threads = Stitchy.DMC.Thread.all

let plaster (w, h) t =
  let open Notty.Infix in
  let (r, g, b) = Stitchy.DMC.Thread.to_rgb t in
  (Notty.I.string Notty.A.empty @@ Format.asprintf "(%d, %d, %d) #%02x%02x%02x" r g b r g b)
    <->
  (Notty.I.string Notty.A.empty @@ Stitchy.DMC.Thread.to_string t)
  <->
  (Notty.I.tabulate w (h - 2) (fun _ _ ->
      Notty.I.char Notty.A.(bg @@ rgb_888 ~r ~g ~b) ' ' 1 1
    ))

let bound n =
  min ((List.length threads) - 1) @@ max 0 n

let () =
  let term = Notty_lwt.Term.create () in
  Lwt_main.run @@ (
    let rec aux n =
      match List.nth_opt threads n with
      | None ->
        Notty_lwt.Term.image term @@ Notty.I.string Notty.A.empty @@ Format.asprintf "%d isn't a findable thread" n >>= fun () ->
        aux 0
      | Some thread ->
        Notty_lwt.Term.image term @@ plaster (Notty_lwt.Term.size term) thread >>= fun () ->
        Lwt_stream.last_new (Notty_lwt.Term.events term) >>= function
        | `Key (`Escape, _) -> Notty_lwt.Term.release term
        | `Key (`ASCII 'h', l) when List.mem `Shift l -> aux @@ bound (n - 10)
        | `Key (`Arrow `Left, l) when List.mem `Shift l -> aux @@ bound (n - 10)
        | `Key (`ASCII 'l', l) when List.mem `Shift l -> aux @@ bound (n + 10)
        | `Key (`Arrow `Right, l) when List.mem `Shift l -> aux @@ bound (n + 10)
        | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) -> aux @@ bound (n + 1)
        | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) -> aux @@ bound (n - 1)

        | _ -> aux n
    in
    aux 0
  )

