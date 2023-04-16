let scale n = (float_of_int n) /. 255.0

(* relative luminance calculation is as described in Web Content
   Accessibility Guidelines (WCAG) 2.0, retrieved from
   https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef *)
let relative_luminance (r, g, b) : float =
  let r_srgb = scale r
  and g_srgb = scale g
  and b_srgb = scale b
  in
  let adjust n = if n <= 0.03928 then n /. 12.92 else Float.pow ((n +. 0.055) /. 1.055) 2.4 in
  0.2126 *. (adjust r_srgb) +. 0.7152 *. (adjust g_srgb) +. 0.0722 *. (adjust b_srgb)

(* to get a contrast ratio, calculate (RL of the lighter color + 0.05) / (RL of the darker color + 0.05). W3C wants that ratio to be at least 4.5:1. *)
let contrast_ratio a b =
  let cr lighter darker = (lighter +. 0.05) /. (darker +. 0.05) in
  let a_rl = relative_luminance a
  and b_rl = relative_luminance b
  in
  if a_rl >= b_rl then cr a_rl b_rl else cr b_rl a_rl

let ensure_contrast_on_white (r, g, b) =
  if contrast_ratio (r, g, b) (255, 255, 255) >= 4.5 then (scale r, scale g, scale b)
  else 0., 0., 0.
