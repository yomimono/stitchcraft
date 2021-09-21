type color =
  | Black
  | White
  | Red
  | Cyan
  | Purple
  | Green
  | Blue
  | Yellow
  | Orange
  | Brown
  | Light_red
  | Dark_gray
  | Gray_2
  | Light_green
  | Light_blue
  | Light_gray

let cmdliner_enum : (string * color) list = [
  "black",Black;
  "white",White;
  "red",Red;
  "cyan",Cyan;
  "purple",Purple;
  "green",Green;
  "blue",Blue;
  "yellow",Yellow;
  "orange",Orange;
  "brown",Brown;
  "lightred",Light_red;
  "darkgray",Dark_gray;
  "gray2",Gray_2;
  "lightgreen",Light_green;
  "lightblue",Light_blue;
  "lightgray",Light_gray;
]

(* specific RGB values and names from c64-wiki.org entry on color *)
let rgb_of_color = function
  | Black -> (0, 0, 0)
  | White -> (255, 255, 255)
  | Red -> (136, 0, 0)
  | Cyan -> (170, 255, 238)
  | Purple -> (204, 68, 204)
  | Green -> (0, 204, 85)
  | Blue -> (0, 0, 170)
  | Yellow -> (238, 238, 119)
  | Orange -> (221, 136, 85)
  | Brown -> (102, 68, 0)
  | Light_red -> (255, 119, 119)
  | Dark_gray -> (51, 51, 51)
  | Gray_2 -> (119, 119, 119)
  | Light_green -> (170, 255, 102)
  | Light_blue -> (0, 136, 255)
  | Light_gray -> (187, 187, 187)

let or_die = function
  | None -> failwith "color lookup failure - check DMC map"
  | Some t -> t

let dmc_lookup s = Stitchy.DMC.Thread.of_string s |> or_die

(** colors may have multiple threads which could represent them.
    as such, return a list, with the (entirely subjective) best match
    first, and additional good matches if present. *)
let thread_of_color = function
  | Black -> dmc_lookup "310"
  | White -> dmc_lookup "B5200"
  | Red -> dmc_lookup "814"
  | Cyan -> dmc_lookup "3811"
  | Purple -> dmc_lookup "553"
  | Green -> dmc_lookup "911"
  | Blue -> dmc_lookup "796"
            (* yellow is tricky, because the displayed CSS color seems washed-out compared to the JPG sample on c64wiki. *)
  | Yellow ->
      dmc_lookup "727" (* one down in the color family,
                             and a better match to the source image *)
  | Orange -> dmc_lookup "977" (* a good match, imo *)
  | Brown -> dmc_lookup "3781" (* looks good from the card, but might be too red IRL *)
  | Light_red -> dmc_lookup "3341" (* a bit on the pinker end *);
  | Dark_gray -> dmc_lookup "3799" (* it is indeed quite dark. *)
  | Gray_2 -> dmc_lookup "317"
  | Light_green ->
        dmc_lookup "704" (* this one is not a great match but it's the best one I could find.  it's not really light enough. *)
  | Light_blue ->
        dmc_lookup "995" (* looks too dark on the card, but I think it's the right intensity *)
  | Light_gray ->
      dmc_lookup "415"
