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

(** colors may have multiple threads which could represent them.
    as such, return a list, with the (entirely subjective) best match
    first, and additional good matches if present. *)
let thread_of_color = function
  | Black -> [ "DMC 310" ]
  | White -> [
      "DMC B5200" (* "true white" *)
    ; "DMC Blanc" (* more muted and also more generally avail *)
    ]
  | Red -> [
      "DMC 814" (* a dark vibrant red *)
    ; "DMC 221" (* a bit more purple *)
    ]
  | Cyan -> [
    "DMC 3811" (* a better color match, but very dull looking *)
  ; "DMC 964" (* bright enough, but too green *)
    ]
  | Purple -> [ "DMC 553" (* looks on the nose to me *)
              ; "DMC 208" (* a touch more jewel-toned, maybe a worse
                             color match but nice in intensity *)
              ]
  | Green -> [ "DMC 911" (* looks perfect to me *)
             ]
  | Blue -> [ "DMC 796" (* probably too dark, but I can't see a better
                           match that isn't too dark to see.
                           intensity pops nicely in person *)
            ]
            (* yellow is tricky, because the displayed CSS color seems washed-out compared to the JPG sample on c64wiki. *)
  | Yellow -> [
      "DMC 727" (* one down in the color family,
                             and a better match to the source image *)
    ; "DMC 3078" (* matching #eeee77, which is washed out *)
    ]
  | Orange -> [
      "DMC 977" (* a good match, imo *)
    ; "DMC 922" (* maybe just a touch too orange -
                             the original color looks fairly muted,
                             and a little browner *)
    ]
  | Brown -> [
        "DMC 3781" (* looks good from the card, but might be too red IRL *)
      ; 
    ]
  | Light_red -> [ "DMC 3341" (* a bit on the pinker end *);
                   "DMC 352" (* fairly peachy *) ]
  | Dark_gray -> [ "DMC 3799" ] (* it is indeed quite dark. *)
  | Gray_2 -> [ "DMC 317"
              ; "DMC 414" (* this is probably too dark and too blue *)
              ]
  | Light_green -> [
        "DMC 704" (* this one is not a great match but it's the best one I could find.  it's not really light enough. *)
      ; "DMC 907" (* too yellow, probably *)
      ]
  | Light_blue -> [
        "DMC 995" (* looks too dark on the card, but I think it's the right intensity *)
        ; "DMC 3843" (* looks right on the card *)
    ]
  | Light_gray -> [
      "DMC 413" (* looks about right *)
    ; "DMC 318" (* a shade darker than 413, in case that one isn't right *)
    ]

