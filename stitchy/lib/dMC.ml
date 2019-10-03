let name = "DMC"

module Thread : sig
  include Thread.S
  val compare : t -> t -> int
end = struct
  type t = { name : string; (* prose name (e.g. "Lavender-VY DK") *)
             identifier : string; (* floss number (except white & ecru) *)
             rgb : RGB.t;
           } [@@deriving yojson, eq]

  let to_rgb t = t.rgb
  let to_string t = Format.sprintf "DMC %s: %s" t.identifier t.name

  let neighbors _ = []

  let compare a b = String.compare a.identifier b.identifier

  module RGBMap = Map.Make(RGB)
  module StringMap = Map.Make(String)

  let add_thread rgb_keyed identifier_keyed identifier name rgb =
    let rgb_map = RGBMap.add rgb {name; identifier; rgb} rgb_keyed
    and id_map = StringMap.add identifier {name; identifier; rgb} identifier_keyed in
    (rgb_map, id_map)

  (* mappings from schemes/dmc.xml in kxstitch *)
  let (rgb_map, id_map) =
    let rgb = RGBMap.empty and id = StringMap.empty in
    let (rgb, id) = add_thread rgb id "Blanc" "White" (252, 251, 248) in
    let (rgb, id) = add_thread rgb id "B5200" "Snow White" (255, 255, 255) in
    let (rgb, id) = add_thread rgb id "Ecru" "Ecru" (240, 234, 218) in
    let (rgb, id) = add_thread rgb id "150" "Dusty Rose Ult Vy Dk" (171, 2, 73) in
    let (rgb, id) = add_thread rgb id "151" "Dusty Rose Vry Lt" (240, 206, 212) in
    let (rgb, id) = add_thread rgb id "152" "Shell Pink Med Light" (226, 160, 153) in
    let (rgb, id) = add_thread rgb id "153" "Violet Very Light" (230, 204, 217) in
    let (rgb, id) = add_thread rgb id "154" "Grape Very Dark" (87, 36, 51) in
    let (rgb, id) = add_thread rgb id "155" "Blue Violet Med Dark" (152, 145, 182) in
    let (rgb, id) = add_thread rgb id "156" "Blue Violet Med Lt" (163, 174, 209) in
    let (rgb, id) = add_thread rgb id "157" "Cornflower Blue Vy Lt" (187, 195, 217) in
    let (rgb, id) = add_thread rgb id "158" "Cornflower Blu M V D" (76, 82, 110) in
    let (rgb, id) = add_thread rgb id "159" "Blue Gray Light" (199, 202, 215) in
    let (rgb, id) = add_thread rgb id "160" "Blue Gray Medium" (153, 159, 183) in
    let (rgb, id) = add_thread rgb id "161" "Blue Gray" (120, 128, 164) in
    let (rgb, id) = add_thread rgb id "162" "Blue Ultra Very Light" (219, 236, 245) in
    let (rgb, id) = add_thread rgb id "163" "Celadon Green Md" (77, 131, 97) in
    let (rgb, id) = add_thread rgb id "164" "Forest Green Lt" (200, 216, 184) in
    let (rgb, id) = add_thread rgb id "165" "Moss Green Vy Lt" (239, 244, 164) in
    let (rgb, id) = add_thread rgb id "166" "Moss Green Md Lt" (192, 200, 64) in
    let (rgb, id) = add_thread rgb id "167" "Yellow Beige V Dk" (167, 124, 73) in
    let (rgb, id) = add_thread rgb id "168" "Pewter Very Light" (209, 209, 209) in
    let (rgb, id) = add_thread rgb id "169" "Pewter Light" (132, 132, 132) in
    let (rgb, id) = add_thread rgb id "208" "Lavender Very Dark" (131, 91, 139) in
    let (rgb, id) = add_thread rgb id "209" "Lavender Dark" (163, 123, 167) in
    let (rgb, id) = add_thread rgb id "210" "Lavender Medium" (195, 159, 195) in
    let (rgb, id) = add_thread rgb id "211" "Lavender Light" (227, 203, 227) in
    let (rgb, id) = add_thread rgb id "221" "Shell Pink Vy Dk" (136, 62, 67) in
    let (rgb, id) = add_thread rgb id "223" "Shell Pink Light" (204, 132, 124) in
    let (rgb, id) = add_thread rgb id "224" "Shell Pink Very Light" (235, 183, 175) in
    let (rgb, id) = add_thread rgb id "225" "Shell Pink Ult Vy Lt" (255, 223, 213) in
    let (rgb, id) = add_thread rgb id "300" "Mahogany Vy Dk" (111, 47, 0) in
    let (rgb, id) = add_thread rgb id "301" "Mahogany Med" (179, 95, 43) in
    let (rgb, id) = add_thread rgb id "304" "Red Medium" (183, 31, 51) in
    let (rgb, id) = add_thread rgb id "307" "Lemon" (253, 237, 84) in
    let (rgb, id) = add_thread rgb id "309" "Rose Dark" (186, 74, 74) in
    let (rgb, id) = add_thread rgb id "310" "Black" (0, 0, 0) in
    let (rgb, id) = add_thread rgb id "311" "Wedgewood Ult VyDk" (28, 80, 102) in
    let (rgb, id) = add_thread rgb id "312" "Baby Blue Very Dark" (53, 102, 139) in
    let (rgb, id) = add_thread rgb id "315" "Antique Mauve Md Dk" (129, 73, 82) in
    let (rgb, id) = add_thread rgb id "316" "Antique Mauve Med" (183, 115, 127) in
    let (rgb, id) = add_thread rgb id "317" "Pewter Gray" (108, 108, 108) in
    let (rgb, id) = add_thread rgb id "318" "Steel Gray Lt" (171, 171, 171) in
    let (rgb, id) = add_thread rgb id "319" "Pistachio Grn Vy Dk" (32, 95, 46) in
    let (rgb, id) = add_thread rgb id "320" "Pistachio Green Med" (105, 136, 90) in
    let (rgb, id) = add_thread rgb id "321" "Red" (199, 43, 59) in
    let (rgb, id) = add_thread rgb id "322" "Baby Blue Dark" (90, 143, 184) in
    let (rgb, id) = add_thread rgb id "326" "Rose Very Dark" (179, 59, 75) in
    let (rgb, id) = add_thread rgb id "327" "Violet Dark" (99, 54, 102) in
    let (rgb, id) = add_thread rgb id "333" "Blue Violet Very Dark" (92, 84, 120) in
    let (rgb, id) = add_thread rgb id "334" "Baby Blue Medium" (115, 159, 193) in
    let (rgb, id) = add_thread rgb id "335" "Rose" (238, 84, 110) in
    let (rgb, id) = add_thread rgb id "336" "Navy Blue" (37, 59, 115) in
    let (rgb, id) = add_thread rgb id "340" "Blue Violet Medium" (173, 167, 199) in
    let (rgb, id) = add_thread rgb id "341" "Blue Violet Light" (183, 191, 221) in
    let (rgb, id) = add_thread rgb id "347" "Salmon Very Dark" (191, 45, 45) in
    let (rgb, id) = add_thread rgb id "349" "Coral Dark" (210, 16, 53) in
    let (rgb, id) = add_thread rgb id "350" "Coral Medium" (224, 72, 72) in
    let (rgb, id) = add_thread rgb id "351" "Coral" (233, 106, 103) in
    let (rgb, id) = add_thread rgb id "352" "Coral Light" (253, 156, 151) in
    let (rgb, id) = add_thread rgb id "353" "Peach" (254, 215, 204) in
    let (rgb, id) = add_thread rgb id "355" "Terra Cotta Dark" (152, 68, 54) in
    let (rgb, id) = add_thread rgb id "356" "Terra Cotta Med" (197, 106, 91) in
    let (rgb, id) = add_thread rgb id "367" "Pistachio Green Dk" (97, 122, 82) in
    let (rgb, id) = add_thread rgb id "368" "Pistachio Green Lt" (166, 194, 152) in
    let (rgb, id) = add_thread rgb id "369" "Pistachio Green Vy Lt" (215, 237, 204) in
    let (rgb, id) = add_thread rgb id "370" "Mustard Medium" (184, 157, 100) in
    let (rgb, id) = add_thread rgb id "371" "Mustard" (191, 166, 113) in
    let (rgb, id) = add_thread rgb id "372" "Mustard Lt" (204, 183, 132) in
    let (rgb, id) = add_thread rgb id "400" "Mahogany Dark" (143, 67, 15) in
    let (rgb, id) = add_thread rgb id "402" "Mahogany Vy Lt" (247, 167, 119) in
    let (rgb, id) = add_thread rgb id "407" "Desert Sand Med" (187, 129, 97) in
    let (rgb, id) = add_thread rgb id "413" "Pewter Gray Dark" (86, 86, 86) in
    let (rgb, id) = add_thread rgb id "414" "Steel Gray Dk" (140, 140, 140) in
    let (rgb, id) = add_thread rgb id "415" "Pearl Gray" (211, 211, 214) in
    let (rgb, id) = add_thread rgb id "420" "Hazelnut Brown Dk" (160, 112, 66) in
    let (rgb, id) = add_thread rgb id "422" "Hazelnut Brown Lt" (198, 159, 123) in
    let (rgb, id) = add_thread rgb id "433" "Brown Med" (122, 69, 31) in
    let (rgb, id) = add_thread rgb id "434" "Brown Light" (152, 94, 51) in
    let (rgb, id) = add_thread rgb id "435" "Brown Very Light" (184, 119, 72) in
    let (rgb, id) = add_thread rgb id "436" "Tan" (203, 144, 81) in
    let (rgb, id) = add_thread rgb id "437" "Tan Light" (228, 187, 142) in
    let (rgb, id) = add_thread rgb id "444" "Lemon Dark" (255, 214, 0) in
    let (rgb, id) = add_thread rgb id "445" "Lemon Light" (255, 251, 139) in
    let (rgb, id) = add_thread rgb id "451" "Shell Gray Dark" (145, 123, 115) in
    let (rgb, id) = add_thread rgb id "452" "Shell Gray Med" (192, 179, 174) in
    let (rgb, id) = add_thread rgb id "453" "Shell Gray Light" (215, 206, 203) in
    let (rgb, id) = add_thread rgb id "469" "Avocado Green" (114, 132, 60) in
    let (rgb, id) = add_thread rgb id "470" "Avocado Grn Lt" (148, 171, 79) in
    let (rgb, id) = add_thread rgb id "471" "Avocado Grn V Lt" (174, 191, 121) in
    let (rgb, id) = add_thread rgb id "472" "Avocado Grn U Lt" (216, 228, 152) in
    let (rgb, id) = add_thread rgb id "498" "Red Dark" (167, 19, 43) in
    let (rgb, id) = add_thread rgb id "500" "Blue Green Vy Dk" (4, 77, 51) in
    let (rgb, id) = add_thread rgb id "501" "Blue Green Dark" (57, 111, 82) in
    let (rgb, id) = add_thread rgb id "502" "Blue Green" (91, 144, 113) in
    let (rgb, id) = add_thread rgb id "503" "Blue Green Med" (123, 172, 148) in
    let (rgb, id) = add_thread rgb id "504" "Blue Green Vy Lt" (196, 222, 204) in
    let (rgb, id) = add_thread rgb id "505" "Jade Green" (51, 131, 98) in
    let (rgb, id) = add_thread rgb id "517" "Wedgewood Dark" (59, 118, 143) in
    let (rgb, id) = add_thread rgb id "518" "Wedgewood Light" (79, 147, 167) in
    let (rgb, id) = add_thread rgb id "519" "Sky Blue" (126, 177, 200) in
    let (rgb, id) = add_thread rgb id "520" "Fern Green Dark" (102, 109, 79) in
    let (rgb, id) = add_thread rgb id "522" "Fern Green" (150, 158, 126) in
    let (rgb, id) = add_thread rgb id "523" "Fern Green Lt" (171, 177, 151) in
    let (rgb, id) = add_thread rgb id "524" "Fern Green Vy Lt" (196, 205, 172) in
    let (rgb, id) = add_thread rgb id "535" "Ash Gray Vy Lt" (99, 100, 88) in
    let (rgb, id) = add_thread rgb id "543" "Beige Brown Ult Vy Lt" (242, 227, 206) in
    let (rgb, id) = add_thread rgb id "550" "Violet Very Dark" (92, 24, 78) in
    let (rgb, id) = add_thread rgb id "552" "Violet  Medium" (128, 58, 107) in
    let (rgb, id) = add_thread rgb id "553" "Violet" (163, 99, 139) in
    let (rgb, id) = add_thread rgb id "554" "Violet Light" (219, 179, 203) in
    let (rgb, id) = add_thread rgb id "561" "Celadon Green VD" (44, 106, 69) in
    let (rgb, id) = add_thread rgb id "562" "Jade Medium" (83, 151, 106) in
    let (rgb, id) = add_thread rgb id "563" "Jade Light" (143, 192, 152) in
    let (rgb, id) = add_thread rgb id "564" "Jade Very Light" (167, 205, 175) in
    let (rgb, id) = add_thread rgb id "580" "Moss Green Dk" (136, 141, 51) in
    let (rgb, id) = add_thread rgb id "581" "Moss Green" (167, 174, 56) in
    let (rgb, id) = add_thread rgb id "597" "Turquoise" (91, 163, 179) in
    let (rgb, id) = add_thread rgb id "598" "Turquoise Light" (144, 195, 204) in
    let (rgb, id) = add_thread rgb id "600" "Cranberry Very Dark" (205, 47, 99) in
    let (rgb, id) = add_thread rgb id "601" "Cranberry Dark" (209, 40, 106) in
    let (rgb, id) = add_thread rgb id "602" "Cranberry Medium" (226, 72, 116) in
    let (rgb, id) = add_thread rgb id "603" "Cranberry" (255, 164, 190) in
    let (rgb, id) = add_thread rgb id "604" "Cranberry Light" (255, 176, 190) in
    let (rgb, id) = add_thread rgb id "605" "Cranberry Very Light" (255, 192, 205) in
    let (rgb, id) = add_thread rgb id "606" "Orange‑Red Bright" (250, 50, 3) in
    let (rgb, id) = add_thread rgb id "608" "Burnt Orange Bright" (253, 93, 53) in
    let (rgb, id) = add_thread rgb id "610" "Drab Brown Dk" (121, 96, 71) in
    let (rgb, id) = add_thread rgb id "611" "Drab Brown" (150, 118, 86) in
    let (rgb, id) = add_thread rgb id "612" "Drab Brown Lt" (188, 154, 120) in
    let (rgb, id) = add_thread rgb id "613" "Drab Brown V Lt" (220, 196, 170) in
    let (rgb, id) = add_thread rgb id "632" "Desert Sand Ult Vy Dk" (135, 85, 57) in
    let (rgb, id) = add_thread rgb id "640" "Beige Gray Vy Dk" (133, 123, 97) in
    let (rgb, id) = add_thread rgb id "642" "Beige Gray Dark" (164, 152, 120) in
    let (rgb, id) = add_thread rgb id "644" "Beige Gray Med" (221, 216, 203) in
    let (rgb, id) = add_thread rgb id "645" "Beaver Gray Vy Dk" (110, 101, 92) in
    let (rgb, id) = add_thread rgb id "646" "Beaver Gray Dk" (135, 125, 115) in
    let (rgb, id) = add_thread rgb id "647" "Beaver Gray Med" (176, 166, 156) in
    let (rgb, id) = add_thread rgb id "648" "Beaver Gray Lt" (188, 180, 172) in
    let (rgb, id) = add_thread rgb id "666" "Bright Red" (227, 29, 66) in
    let (rgb, id) = add_thread rgb id "676" "Old Gold Lt" (229, 206, 151) in
    let (rgb, id) = add_thread rgb id "677" "Old Gold Vy Lt" (245, 236, 203) in
    let (rgb, id) = add_thread rgb id "680" "Old Gold Dark" (188, 141, 14) in
    let (rgb, id) = add_thread rgb id "699" "Green" (5, 101, 23) in
    let (rgb, id) = add_thread rgb id "700" "Green Bright" (7, 115, 27) in
    let (rgb, id) = add_thread rgb id "701" "Green Light" (63, 143, 41) in
    let (rgb, id) = add_thread rgb id "702" "Kelly Green" (71, 167, 47) in
    let (rgb, id) = add_thread rgb id "703" "Chartreuse" (123, 181, 71) in
    let (rgb, id) = add_thread rgb id "704" "Chartreuse Bright" (158, 207, 52) in
    let (rgb, id) = add_thread rgb id "712" "Cream" (255, 251, 239) in
    let (rgb, id) = add_thread rgb id "718" "Plum" (156, 36, 98) in
    let (rgb, id) = add_thread rgb id "720" "Orange Spice Dark" (229, 92, 31) in
    let (rgb, id) = add_thread rgb id "721" "Orange Spice Med" (242, 120, 66) in
    let (rgb, id) = add_thread rgb id "722" "Orange Spice Light" (247, 151, 111) in
    let (rgb, id) = add_thread rgb id "725" "Topaz Med Lt" (255, 200, 64) in
    let (rgb, id) = add_thread rgb id "726" "Topaz Light" (253, 215, 85) in
    let (rgb, id) = add_thread rgb id "727" "Topaz Vy Lt" (255, 241, 175) in
    let (rgb, id) = add_thread rgb id "728" "Topaz" (228, 180, 104) in
    let (rgb, id) = add_thread rgb id "729" "Old Gold Medium" (208, 165, 62) in
    let (rgb, id) = add_thread rgb id "730" "Olive Green V Dk" (130, 123, 48) in
    let (rgb, id) = add_thread rgb id "731" "Olive Green Dk" (147, 139, 55) in
    let (rgb, id) = add_thread rgb id "732" "Olive Green" (148, 140, 54) in
    let (rgb, id) = add_thread rgb id "733" "Olive Green Md" (188, 179, 76) in
    let (rgb, id) = add_thread rgb id "734" "Olive Green Lt" (199, 192, 119) in
    let (rgb, id) = add_thread rgb id "738" "Tan Very Light" (236, 204, 158) in
    let (rgb, id) = add_thread rgb id "739" "Tan Ult Vy Lt" (248, 228, 200) in
    let (rgb, id) = add_thread rgb id "740" "Tangerine" (255, 139, 0) in
    let (rgb, id) = add_thread rgb id "741" "Tangerine Med" (255, 163, 43) in
    let (rgb, id) = add_thread rgb id "742" "Tangerine Light" (255, 191, 87) in
    let (rgb, id) = add_thread rgb id "743" "Yellow Med" (254, 211, 118) in
    let (rgb, id) = add_thread rgb id "744" "Yellow Pale" (255, 231, 147) in
    let (rgb, id) = add_thread rgb id "745" "Yellow Pale Light" (255, 233, 173) in
    let (rgb, id) = add_thread rgb id "746" "Off White" (252, 252, 238) in
    let (rgb, id) = add_thread rgb id "747" "Peacock Blue Vy Lt" (229, 252, 253) in
    let (rgb, id) = add_thread rgb id "754" "Peach Light" (247, 203, 191) in
    let (rgb, id) = add_thread rgb id "758" "Terra Cotta Vy Lt" (238, 170, 155) in
    let (rgb, id) = add_thread rgb id "760" "Salmon" (245, 173, 173) in
    let (rgb, id) = add_thread rgb id "761" "Salmon Light" (255, 201, 201) in
    let (rgb, id) = add_thread rgb id "762" "Pearl Gray Vy Lt" (236, 236, 236) in
    let (rgb, id) = add_thread rgb id "772" "Yellow Green Vy Lt" (228, 236, 212) in
    let (rgb, id) = add_thread rgb id "775" "Baby Blue Very Light" (217, 235, 241) in
    let (rgb, id) = add_thread rgb id "776" "Pink Medium" (252, 176, 185) in
    let (rgb, id) = add_thread rgb id "777" "Raspberry Very Dark" (145, 53, 70) in
    let (rgb, id) = add_thread rgb id "778" "Antique Mauve Vy Lt" (223, 179, 187) in
    let (rgb, id) = add_thread rgb id "779" "Cocoa Dark" (98, 75, 69) in
    let (rgb, id) = add_thread rgb id "780" "Topaz Ultra Vy Dk" (148, 99, 26) in
    let (rgb, id) = add_thread rgb id "781" "Topaz Very Dark" (162, 109, 32) in
    let (rgb, id) = add_thread rgb id "782" "Topaz Dark" (174, 119, 32) in
    let (rgb, id) = add_thread rgb id "783" "Topaz Medium" (206, 145, 36) in
    let (rgb, id) = add_thread rgb id "791" "Cornflower Blue V D" (70, 69, 99) in
    let (rgb, id) = add_thread rgb id "792" "Cornflower Blue Dark" (85, 91, 123) in
    let (rgb, id) = add_thread rgb id "793" "Cornflower Blue Med" (112, 125, 162) in
    let (rgb, id) = add_thread rgb id "794" "Cornflower Blue Light" (143, 156, 193) in
    let (rgb, id) = add_thread rgb id "796" "Royal Blue Dark" (17, 65, 109) in
    let (rgb, id) = add_thread rgb id "797" "Royal Blue" (19, 71, 125) in
    let (rgb, id) = add_thread rgb id "798" "Delft Blue Dark" (70, 106, 142) in
    let (rgb, id) = add_thread rgb id "799" "Delft Blue Medium" (116, 142, 182) in
    let (rgb, id) = add_thread rgb id "800" "Delft Blue Pale" (192, 204, 222) in
    let (rgb, id) = add_thread rgb id "801" "Coffee Brown Dk" (101, 57, 25) in
    let (rgb, id) = add_thread rgb id "803" "Baby Blue Ult Vy Dk" (44, 89, 124) in
    let (rgb, id) = add_thread rgb id "806" "Peacock Blue Dark" (61, 149, 165) in
    let (rgb, id) = add_thread rgb id "807" "Peacock Blue" (100, 171, 186) in
    let (rgb, id) = add_thread rgb id "809" "Delft Blue" (148, 168, 198) in
    let (rgb, id) = add_thread rgb id "813" "Blue Light" (161, 194, 215) in
    let (rgb, id) = add_thread rgb id "814" "Garnet Dark" (123, 0, 27) in
    let (rgb, id) = add_thread rgb id "815" "Garnet Medium" (135, 7, 31) in
    let (rgb, id) = add_thread rgb id "816" "Garnet" (151, 11, 35) in
    let (rgb, id) = add_thread rgb id "817" "Coral Red Very Dark" (187, 5, 31) in
    let (rgb, id) = add_thread rgb id "818" "Baby Pink" (255, 223, 217) in
    let (rgb, id) = add_thread rgb id "819" "Baby Pink Light" (255, 238, 235) in
    let (rgb, id) = add_thread rgb id "820" "Royal Blue Very Dark" (14, 54, 92) in
    let (rgb, id) = add_thread rgb id "822" "Beige Gray Light" (231, 226, 211) in
    let (rgb, id) = add_thread rgb id "823" "Navy Blue Dark" (33, 48, 99) in
    let (rgb, id) = add_thread rgb id "824" "Blue Very Dark" (57, 105, 135) in
    let (rgb, id) = add_thread rgb id "825" "Blue Dark" (71, 129, 165) in
    let (rgb, id) = add_thread rgb id "826" "Blue Medium" (107, 158, 191) in
    let (rgb, id) = add_thread rgb id "827" "Blue Very Light" (189, 221, 237) in
    let (rgb, id) = add_thread rgb id "828" "Sky Blue Vy Lt" (197, 232, 237) in
    let (rgb, id) = add_thread rgb id "829" "Golden Olive Vy Dk" (126, 107, 66) in
    let (rgb, id) = add_thread rgb id "830" "Golden Olive Dk" (141, 120, 75) in
    let (rgb, id) = add_thread rgb id "831" "Golden Olive Md" (170, 143, 86) in
    let (rgb, id) = add_thread rgb id "832" "Golden Olive" (189, 155, 81) in
    let (rgb, id) = add_thread rgb id "833" "Golden Olive Lt" (200, 171, 108) in
    let (rgb, id) = add_thread rgb id "834" "Golden Olive Vy Lt" (219, 190, 127) in
    let (rgb, id) = add_thread rgb id "838" "Beige Brown Vy Dk" (89, 73, 55) in
    let (rgb, id) = add_thread rgb id "839" "Beige Brown Dk" (103, 85, 65) in
    let (rgb, id) = add_thread rgb id "840" "Beige Brown Med" (154, 124, 92) in
    let (rgb, id) = add_thread rgb id "841" "Beige Brown Lt" (182, 155, 126) in
    let (rgb, id) = add_thread rgb id "842" "Beige Brown Vy Lt" (209, 186, 161) in
    let (rgb, id) = add_thread rgb id "844" "Beaver Gray Ult Dk" (72, 72, 72) in
    let (rgb, id) = add_thread rgb id "869" "Hazelnut Brown V Dk" (131, 94, 57) in
    let (rgb, id) = add_thread rgb id "890" "Pistachio Grn Ult V D" (23, 73, 35) in
    let (rgb, id) = add_thread rgb id "891" "Carnation Dark" (255, 87, 115) in
    let (rgb, id) = add_thread rgb id "892" "Carnation Medium" (255, 121, 140) in
    let (rgb, id) = add_thread rgb id "893" "Carnation Light" (252, 144, 162) in
    let (rgb, id) = add_thread rgb id "894" "Carnation Very Light" (255, 178, 187) in
    let (rgb, id) = add_thread rgb id "895" "Hunter Green Vy Dk" (27, 83, 0) in
    let (rgb, id) = add_thread rgb id "898" "Coffee Brown Vy Dk" (73, 42, 19) in
    let (rgb, id) = add_thread rgb id "899" "Rose Medium" (242, 118, 136) in
    let (rgb, id) = add_thread rgb id "900" "Burnt Orange Dark" (209, 88, 7) in
    let (rgb, id) = add_thread rgb id "902" "Garnet Very Dark" (130, 38, 55) in
    let (rgb, id) = add_thread rgb id "904" "Parrot Green V Dk" (85, 120, 34) in
    let (rgb, id) = add_thread rgb id "905" "Parrot Green Dk" (98, 138, 40) in
    let (rgb, id) = add_thread rgb id "906" "Parrot Green Md" (127, 179, 53) in
    let (rgb, id) = add_thread rgb id "907" "Parrot Green Lt" (199, 230, 102) in
    let (rgb, id) = add_thread rgb id "909" "Emerald Green Vy Dk" (21, 111, 73) in
    let (rgb, id) = add_thread rgb id "910" "Emerald Green Dark" (24, 126, 86) in
    let (rgb, id) = add_thread rgb id "911" "Emerald Green Med" (24, 144, 101) in
    let (rgb, id) = add_thread rgb id "912" "Emerald Green Lt" (27, 157, 107) in
    let (rgb, id) = add_thread rgb id "913" "Nile Green Med" (109, 171, 119) in
    let (rgb, id) = add_thread rgb id "915" "Plum Dark" (130, 0, 67) in
    let (rgb, id) = add_thread rgb id "917" "Plum Medium" (155, 19, 89) in
    let (rgb, id) = add_thread rgb id "918" "Red‑Copper Dark" (130, 52, 10) in
    let (rgb, id) = add_thread rgb id "919" "Red‑Copper" (166, 69, 16) in
    let (rgb, id) = add_thread rgb id "920" "Copper Med" (172, 84, 20) in
    let (rgb, id) = add_thread rgb id "921" "Copper" (198, 98, 24) in
    let (rgb, id) = add_thread rgb id "922" "Copper Light" (226, 115, 35) in
    let (rgb, id) = add_thread rgb id "924" "Gray Green Vy Dark" (86, 106, 106) in
    let (rgb, id) = add_thread rgb id "926" "Gray Green Med" (152, 174, 174) in
    let (rgb, id) = add_thread rgb id "927" "Gray Green Light" (189, 203, 203) in
    let (rgb, id) = add_thread rgb id "928" "Gray Green Vy Lt" (221, 227, 227) in
    let (rgb, id) = add_thread rgb id "930" "Antique Blue Dark" (69, 92, 113) in
    let (rgb, id) = add_thread rgb id "931" "Antique Blue Medium" (106, 133, 158) in
    let (rgb, id) = add_thread rgb id "932" "Antique Blue Light" (162, 181, 198) in
    let (rgb, id) = add_thread rgb id "934" "Avocado Grn Black" (49, 57, 25) in
    let (rgb, id) = add_thread rgb id "935" "Avocado Green Dk" (66, 77, 33) in
    let (rgb, id) = add_thread rgb id "936" "Avocado Grn V Dk" (76, 88, 38) in
    let (rgb, id) = add_thread rgb id "937" "Avocado Green Md" (98, 113, 51) in
    let (rgb, id) = add_thread rgb id "938" "Coffee Brown Ult Dk" (54, 31, 14) in
    let (rgb, id) = add_thread rgb id "939" "Navy Blue Very Dark" (27, 40, 83) in
    let (rgb, id) = add_thread rgb id "943" "Green Bright Md" (61, 147, 132) in
    let (rgb, id) = add_thread rgb id "945" "Tawny" (251, 213, 187) in
    let (rgb, id) = add_thread rgb id "946" "Burnt Orange Med" (235, 99, 7) in
    let (rgb, id) = add_thread rgb id "947" "Burnt Orange" (255, 123, 77) in
    let (rgb, id) = add_thread rgb id "948" "Peach Very Light" (254, 231, 218) in
    let (rgb, id) = add_thread rgb id "950" "Desert Sand Light" (238, 211, 196) in
    let (rgb, id) = add_thread rgb id "951" "Tawny Light" (255, 226, 207) in
    let (rgb, id) = add_thread rgb id "954" "Nile Green" (136, 186, 145) in
    let (rgb, id) = add_thread rgb id "955" "Nile Green Light" (162, 214, 173) in
    let (rgb, id) = add_thread rgb id "956" "Geranium" (255, 145, 145) in
    let (rgb, id) = add_thread rgb id "957" "Geranium Pale" (253, 181, 181) in
    let (rgb, id) = add_thread rgb id "958" "Sea Green Dark" (62, 182, 161) in
    let (rgb, id) = add_thread rgb id "959" "Sea Green Med" (89, 199, 180) in
    let (rgb, id) = add_thread rgb id "961" "Dusty Rose Dark" (207, 115, 115) in
    let (rgb, id) = add_thread rgb id "962" "Dusty Rose Medium" (230, 138, 138) in
    let (rgb, id) = add_thread rgb id "963" "Dusty Rose Ult Vy Lt" (255, 215, 215) in
    let (rgb, id) = add_thread rgb id "964" "Sea Green Light" (169, 226, 216) in
    let (rgb, id) = add_thread rgb id "966" "Jade Ultra Vy Lt" (185, 215, 192) in
    let (rgb, id) = add_thread rgb id "967" "Apricot Very Light" (255, 222, 213) in
    let (rgb, id) = add_thread rgb id "970" "Pumpkin Light" (247, 139, 19) in
    let (rgb, id) = add_thread rgb id "971" "Pumpkin" (246, 127, 0) in
    let (rgb, id) = add_thread rgb id "972" "Canary Deep" (255, 181, 21) in
    let (rgb, id) = add_thread rgb id "973" "Canary Bright" (255, 227, 0) in
    let (rgb, id) = add_thread rgb id "975" "Golden Brown Dk" (145, 79, 18) in
    let (rgb, id) = add_thread rgb id "976" "Golden Brown Med" (194, 129, 66) in
    let (rgb, id) = add_thread rgb id "977" "Golden Brown Light" (220, 156, 86) in
    let (rgb, id) = add_thread rgb id "986" "Forest Green Vy Dk" (64, 82, 48) in
    let (rgb, id) = add_thread rgb id "987" "Forest Green Dk" (88, 113, 65) in
    let (rgb, id) = add_thread rgb id "988" "Forest Green Med" (115, 139, 91) in
    let (rgb, id) = add_thread rgb id "989" "Forest Green " (141, 166, 117) in
    let (rgb, id) = add_thread rgb id "991" "Aquamarine Dk" (71, 123, 110) in
    let (rgb, id) = add_thread rgb id "992" "Aquamarine Lt" (111, 174, 159) in
    let (rgb, id) = add_thread rgb id "993" "Aquamarine Vy Lt" (144, 192, 180) in
    let (rgb, id) = add_thread rgb id "995" "Electric Blue Dark" (38, 150, 182) in
    let (rgb, id) = add_thread rgb id "996" "Electric Blue Medium" (48, 194, 236) in
    let (rgb, id) = add_thread rgb id "3011" "Khaki Green Dk" (137, 138, 88) in
    let (rgb, id) = add_thread rgb id "3012" "Khaki Green Md" (166, 167, 93) in
    let (rgb, id) = add_thread rgb id "3013" "Khaki Green Lt" (185, 185, 130) in
    let (rgb, id) = add_thread rgb id "3021" "Brown Gray Vy Dk" (79, 75, 65) in
    let (rgb, id) = add_thread rgb id "3022" "Brown Gray Med" (142, 144, 120) in
    let (rgb, id) = add_thread rgb id "3023" "Brown Gray Light" (177, 170, 151) in
    let (rgb, id) = add_thread rgb id "3024" "Brown Gray Vy Lt" (235, 234, 231) in
    let (rgb, id) = add_thread rgb id "3031" "Mocha Brown Vy Dk" (75, 60, 42) in
    let (rgb, id) = add_thread rgb id "3032" "Mocha Brown Med" (179, 159, 139) in
    let (rgb, id) = add_thread rgb id "3033" "Mocha Brown Vy Lt" (227, 216, 204) in
    let (rgb, id) = add_thread rgb id "3041" "Antique Violet Medium" (149, 111, 124) in
    let (rgb, id) = add_thread rgb id "3042" "Antique Violet Light" (183, 157, 167) in
    let (rgb, id) = add_thread rgb id "3045" "Yellow Beige Dk" (188, 150, 106) in
    let (rgb, id) = add_thread rgb id "3046" "Yellow Beige Md" (216, 188, 154) in
    let (rgb, id) = add_thread rgb id "3047" "Yellow Beige Lt" (231, 214, 193) in
    let (rgb, id) = add_thread rgb id "3051" "Green Gray Dk" (95, 102, 72) in
    let (rgb, id) = add_thread rgb id "3052" "Green Gray Md" (136, 146, 104) in
    let (rgb, id) = add_thread rgb id "3053" "Green Gray" (156, 164, 130) in
    let (rgb, id) = add_thread rgb id "3064" "Desert Sand" (196, 142, 112) in
    let (rgb, id) = add_thread rgb id "3072" "Beaver Gray Vy Lt" (230, 232, 232) in
    let (rgb, id) = add_thread rgb id "3078" "Golden Yellow Vy Lt" (253, 249, 205) in
    let (rgb, id) = add_thread rgb id "3325" "Baby Blue Light" (184, 210, 230) in
    let (rgb, id) = add_thread rgb id "3326" "Rose Light" (251, 173, 180) in
    let (rgb, id) = add_thread rgb id "3328" "Salmon Dark" (227, 109, 109) in
    let (rgb, id) = add_thread rgb id "3340" "Apricot Med" (255, 131, 111) in
    let (rgb, id) = add_thread rgb id "3341" "Apricot" (252, 171, 152) in
    let (rgb, id) = add_thread rgb id "3345" "Hunter Green Dk" (27, 89, 21) in
    let (rgb, id) = add_thread rgb id "3346" "Hunter Green" (64, 106, 58) in
    let (rgb, id) = add_thread rgb id "3347" "Yellow Green Med" (113, 147, 92) in
    let (rgb, id) = add_thread rgb id "3348" "Yellow Green Lt" (204, 217, 177) in
    let (rgb, id) = add_thread rgb id "3350" "Dusty Rose Ultra Dark" (188, 67, 101) in
    let (rgb, id) = add_thread rgb id "3354" "Dusty Rose Light" (228, 166, 172) in
    let (rgb, id) = add_thread rgb id "3362" "Pine Green Dk" (94, 107, 71) in
    let (rgb, id) = add_thread rgb id "3363" "Pine Green Md" (114, 130, 86) in
    let (rgb, id) = add_thread rgb id "3364" "Pine Green" (131, 151, 95) in
    let (rgb, id) = add_thread rgb id "3371" "Black Brown" (30, 17, 8) in
    let (rgb, id) = add_thread rgb id "3607" "Plum Light" (197, 73, 137) in
    let (rgb, id) = add_thread rgb id "3608" "Plum Very Light" (234, 156, 196) in
    let (rgb, id) = add_thread rgb id "3609" "Plum Ultra Light" (244, 174, 213) in
    let (rgb, id) = add_thread rgb id "3685" "Mauve Very Dark" (136, 21, 49) in
    let (rgb, id) = add_thread rgb id "3687" "Mauve" (201, 107, 112) in
    let (rgb, id) = add_thread rgb id "3688" "Mauve Medium" (231, 169, 172) in
    let (rgb, id) = add_thread rgb id "3689" "Mauve Light" (251, 191, 194) in
    let (rgb, id) = add_thread rgb id "3705" "Melon Dark" (255, 121, 146) in
    let (rgb, id) = add_thread rgb id "3706" "Melon Medium" (255, 173, 188) in
    let (rgb, id) = add_thread rgb id "3708" "Melon Light" (255, 203, 213) in
    let (rgb, id) = add_thread rgb id "3712" "Salmon Medium" (241, 135, 135) in
    let (rgb, id) = add_thread rgb id "3713" "Salmon Very Light" (255, 226, 226) in
    let (rgb, id) = add_thread rgb id "3716" "Dusty Rose Med Vy Lt" (255, 189, 189) in
    let (rgb, id) = add_thread rgb id "3721" "Shell Pink Dark" (161, 75, 81) in
    let (rgb, id) = add_thread rgb id "3722" "Shell Pink Med" (188, 108, 100) in
    let (rgb, id) = add_thread rgb id "3726" "Antique Mauve Dark" (155, 91, 102) in
    let (rgb, id) = add_thread rgb id "3727" "Antique Mauve Light" (219, 169, 178) in
    let (rgb, id) = add_thread rgb id "3731" "Dusty Rose Very Dark" (218, 103, 131) in
    let (rgb, id) = add_thread rgb id "3733" "Dusty Rose" (232, 135, 155) in
    let (rgb, id) = add_thread rgb id "3740" "Antique Violet Dark" (120, 87, 98) in
    let (rgb, id) = add_thread rgb id "3743" "Antique Violet Vy Lt" (215, 203, 211) in
    let (rgb, id) = add_thread rgb id "3746" "Blue Violet Dark" (119, 107, 152) in
    let (rgb, id) = add_thread rgb id "3747" "Blue Violet Vy Lt" (211, 215, 237) in
    let (rgb, id) = add_thread rgb id "3750" "Antique Blue Very Dk" (56, 76, 94) in
    let (rgb, id) = add_thread rgb id "3752" "Antique Blue Very Lt" (199, 209, 219) in
    let (rgb, id) = add_thread rgb id "3753" "Antique Blue Ult Vy Lt" (219, 226, 233) in
    let (rgb, id) = add_thread rgb id "3755" "Baby Blue" (147, 180, 206) in
    let (rgb, id) = add_thread rgb id "3756" "Baby Blue Ult Vy Lt" (238, 252, 252) in
    let (rgb, id) = add_thread rgb id "3760" "Wedgewood Med" (62, 133, 162) in
    let (rgb, id) = add_thread rgb id "3761" "Sky Blue Light" (172, 216, 226) in
    let (rgb, id) = add_thread rgb id "3765" "Peacock Blue Vy Dk" (52, 127, 140) in
    let (rgb, id) = add_thread rgb id "3766" "Peacock Blue Light" (153, 207, 217) in
    let (rgb, id) = add_thread rgb id "3768" "Gray Green Dark" (101, 127, 127) in
    let (rgb, id) = add_thread rgb id "3770" "Tawny Vy Light" (255, 238, 227) in
    let (rgb, id) = add_thread rgb id "3771" "Terra Cotta Ult Vy Lt" (244, 187, 169) in
    let (rgb, id) = add_thread rgb id "3772" "Desert Sand Vy Dk" (160, 108, 80) in
    let (rgb, id) = add_thread rgb id "3773" "Desert Sand Dark" (182, 117, 82) in
    let (rgb, id) = add_thread rgb id "3774" "Desert Sand Vy Lt" (243, 225, 215) in
    let (rgb, id) = add_thread rgb id "3776" "Mahogany Light" (207, 121, 57) in
    let (rgb, id) = add_thread rgb id "3777" "Terra Cotta Vy Dk" (134, 48, 34) in
    let (rgb, id) = add_thread rgb id "3778" "Terra Cotta Light" (217, 137, 120) in
    let (rgb, id) = add_thread rgb id "3779" "Rosewood Ult Vy Lt" (248, 202, 200) in
    let (rgb, id) = add_thread rgb id "3781" "Mocha Brown Dk" (107, 87, 67) in
    let (rgb, id) = add_thread rgb id "3782" "Mocha Brown Lt" (210, 188, 166) in
    let (rgb, id) = add_thread rgb id "3787" "Brown Gray Dark" (98, 93, 80) in
    let (rgb, id) = add_thread rgb id "3790" "Beige Gray Ult Dk" (127, 106, 85) in
    let (rgb, id) = add_thread rgb id "3799" "Pewter Gray Vy Dk" (66, 66, 66) in
    let (rgb, id) = add_thread rgb id "3801" "Melon Very Dark" (231, 73, 103) in
    let (rgb, id) = add_thread rgb id "3802" "Antique Mauve Vy Dk" (113, 65, 73) in
    let (rgb, id) = add_thread rgb id "3803" "Mauve Dark" (171, 51, 87) in
    let (rgb, id) = add_thread rgb id "3804" "Cyclamen Pink Dark" (224, 40, 118) in
    let (rgb, id) = add_thread rgb id "3805" "Cyclamen Pink" (243, 71, 139) in
    let (rgb, id) = add_thread rgb id "3806" "Cyclamen Pink Light" (255, 140, 174) in
    let (rgb, id) = add_thread rgb id "3807" "Cornflower Blue" (96, 103, 140) in
    let (rgb, id) = add_thread rgb id "3808" "Turquoise Ult Vy Dk" (54, 105, 112) in
    let (rgb, id) = add_thread rgb id "3809" "Turquoise Vy Dark" (63, 124, 133) in
    let (rgb, id) = add_thread rgb id "3810" "Turquoise Dark" (72, 142, 154) in
    let (rgb, id) = add_thread rgb id "3811" "Turquoise Very Light" (188, 227, 230) in
    let (rgb, id) = add_thread rgb id "3812" "Sea Green Vy Dk" (47, 140, 132) in
    let (rgb, id) = add_thread rgb id "3813" "Blue Green Lt" (178, 212, 189) in
    let (rgb, id) = add_thread rgb id "3814" "Aquamarine" (80, 139, 125) in
    let (rgb, id) = add_thread rgb id "3815" "Celadon Green Dk" (71, 119, 89) in
    let (rgb, id) = add_thread rgb id "3816" "Celadon Green" (101, 165, 125) in
    let (rgb, id) = add_thread rgb id "3817" "Celadon Green Lt" (153, 195, 170) in
    let (rgb, id) = add_thread rgb id "3818" "Emerald Grn Ult V Dk" (17, 90, 59) in
    let (rgb, id) = add_thread rgb id "3819" "Moss Green Lt" (224, 232, 104) in
    let (rgb, id) = add_thread rgb id "3820" "Straw Dark" (223, 182, 95) in
    let (rgb, id) = add_thread rgb id "3821" "Straw" (243, 206, 117) in
    let (rgb, id) = add_thread rgb id "3822" "Straw Light" (246, 220, 152) in
    let (rgb, id) = add_thread rgb id "3823" "Yellow Ultra Pale" (255, 253, 227) in
    let (rgb, id) = add_thread rgb id "3824" "Apricot Light" (254, 205, 194) in
    let (rgb, id) = add_thread rgb id "3825" "Pumpkin Pale" (253, 189, 150) in
    let (rgb, id) = add_thread rgb id "3826" "Golden Brown" (173, 114, 57) in
    let (rgb, id) = add_thread rgb id "3827" "Golden Brown Pale" (247, 187, 119) in
    let (rgb, id) = add_thread rgb id "3828" "Hazelnut Brown" (183, 139, 97) in
    let (rgb, id) = add_thread rgb id "3829" "Old Gold Vy Dark" (169, 130, 4) in
    let (rgb, id) = add_thread rgb id "3830" "Terra Cotta" (185, 85, 68) in
    let (rgb, id) = add_thread rgb id "3831" "Raspberry Dark" (179, 47, 72) in
    let (rgb, id) = add_thread rgb id "3832" "Raspberry Medium" (219, 85, 110) in
    let (rgb, id) = add_thread rgb id "3833" "Raspberry Light" (234, 134, 153) in
    let (rgb, id) = add_thread rgb id "3834" "Grape Dark" (114, 55, 93) in
    let (rgb, id) = add_thread rgb id "3835" "Grape Medium" (148, 96, 131) in
    let (rgb, id) = add_thread rgb id "3836" "Grape Light" (186, 145, 170) in
    let (rgb, id) = add_thread rgb id "3837" "Lavender Ultra Dark" (108, 58, 110) in
    let (rgb, id) = add_thread rgb id "3838" "Lavender Blue Dark" (92, 114, 148) in
    let (rgb, id) = add_thread rgb id "3839" "Lavender Blue Med" (123, 142, 171) in
    let (rgb, id) = add_thread rgb id "3840" "Lavender Blue Light" (176, 192, 218) in
    let (rgb, id) = add_thread rgb id "3841" "Baby Blue Pale" (205, 223, 237) in
    let (rgb, id) = add_thread rgb id "3842" "Wedgewood Vry Dk" (50, 102, 124) in
    let (rgb, id) = add_thread rgb id "3843" "Electric Blue" (20, 170, 208) in
    let (rgb, id) = add_thread rgb id "3844" "Turquoise Bright Dark" (18, 174, 186) in
    let (rgb, id) = add_thread rgb id "3845" "Turquoise Bright Med" (4, 196, 202) in
    let (rgb, id) = add_thread rgb id "3846" "Turquoise Bright Light" (6, 227, 230) in
    let (rgb, id) = add_thread rgb id "3847" "Teal Green Dark" (52, 125, 117) in
    let (rgb, id) = add_thread rgb id "3848" "Teal Green Med" (85, 147, 146) in
    let (rgb, id) = add_thread rgb id "3849" "Teal Green Light" (82, 179, 164) in
    let (rgb, id) = add_thread rgb id "3850" "Green Bright Dk" (55, 132, 119) in
    let (rgb, id) = add_thread rgb id "3851" "Green Bright Lt" (73, 179, 161) in
    let (rgb, id) = add_thread rgb id "3852" "Straw Very Dark" (205, 157, 55) in
    let (rgb, id) = add_thread rgb id "3853" "Autumn Gold Dk" (242, 151, 70) in
    let (rgb, id) = add_thread rgb id "3854" "Autumn Gold Med" (242, 175, 104) in
    let (rgb, id) = add_thread rgb id "3855" "Autumn Gold Lt" (250, 211, 150) in
    let (rgb, id) = add_thread rgb id "3856" "Mahogany Ult Vy Lt" (255, 211, 181) in
    let (rgb, id) = add_thread rgb id "3857" "Rosewood Dark" (104, 37, 26) in
    let (rgb, id) = add_thread rgb id "3858" "Rosewood Med" (150, 74, 63) in
    let (rgb, id) = add_thread rgb id "3859" "Rosewood Light" (186, 139, 124) in
    let (rgb, id) = add_thread rgb id "3860" "Cocoa" (125, 93, 87) in
    let (rgb, id) = add_thread rgb id "3861" "Cocoa Light" (166, 136, 129) in
    let (rgb, id) = add_thread rgb id "3862" "Mocha Beige Dark" (138, 110, 78) in
    let (rgb, id) = add_thread rgb id "3863" "Mocha Beige Med" (164, 131, 92) in
    let (rgb, id) = add_thread rgb id "3864" "Mocha Beige Light" (203, 182, 156) in
    let (rgb, id) = add_thread rgb id "3865" "Winter White" (249, 247, 241) in
    let (rgb, id) = add_thread rgb id "3866" "Mocha Brn Ult Vy Lt" (250, 246, 240) in
    (rgb, id)

  let basic = [
    StringMap.find "310" id_map; (* black *)
    StringMap.find "816" id_map; (* garnet *)
    StringMap.find "900" id_map; (* orange *)
    StringMap.find "743" id_map; (* yellow *)
    StringMap.find "166" id_map; (* green *)
  ]

  let to_crowbar =
    let l = RGBMap.bindings rgb_map in
    Crowbar.choose (List.map (fun (_k, v) -> Crowbar.const v) l)

  let of_rgb color = RGBMap.find_opt color rgb_map
end
