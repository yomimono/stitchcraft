open Angstrom

type pattern = {
  version : int;
  width : int;
  height : int;
  u1 : int; (* I don't know what this means, but it's pretty much always 35328 (0x8a00) *)
  u2 : int; (* consistently 273 (0x111) *)
  u3 : int; (* this varies, but I haven't figured out what it means *)
  u4 : int; (* this varies but is usually small; I initially thought it was a color count, but that doesn't match up with test data *)
}

let pp_dechex fmt n =
  Format.fprintf fmt "%d (0x%x)" n n

let pp_pattern fmt pattern =
  Format.fprintf fmt "version %d; %a by %a grid; unknowns are %a %a %a %a"
    pattern.version
    pp_dechex pattern.width
    pp_dechex pattern.height
    pp_dechex pattern.u1
    pp_dechex pattern.u2
    pp_dechex pattern.u3
    pp_dechex pattern.u4

let magic = string "PCStitch "
let version = any_char
let more_magic = string " Pattern File"
let skip_spaces = skip_while (fun c -> Char.compare c ' ' = 0)

let header =
  (magic *> version) <* more_magic
   <?>
   "file header incorrect -- is this a .pat file?"

module V7 = struct
  let width = LE.any_uint16
  and height = LE.any_uint16

  let post_header_padding =
    skip_spaces *> int8 0

  let unknown_1_after_magic = LE.any_uint16
  let unknown_2_after_magic = LE.any_uint16
                    
  let nulls = skip_while (fun c -> Char.compare c '\000' = 0)

  let unknown_3_after_magic = LE.any_uint16
  let unknown_4_after_magic = LE.any_uint16

  let known_postheader =
    post_header_padding *>
    unknown_1_after_magic >>= fun u1 ->
    unknown_2_after_magic >>= fun u2 ->
    nulls >>= fun () ->
    unknown_3_after_magic >>= fun u3 ->
    unknown_4_after_magic >>= fun u4 ->
    width >>= fun width ->
    height >>= fun height ->
    return {version = 7;
            u1; u2; u3; u4; width; height}

end


let file =
  header >>= function
  | '7' -> V7.known_postheader
  | _ -> fail "unknown file format -- try a version 7 file"
