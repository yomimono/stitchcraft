type symbol = | Symbol of string | Zapf of string

let symbols = [
   (* unicode 0x23*) Symbol "\x23";
   (* unicode 0x25*) Symbol "\x25";
   (* unicode 0x26*) Symbol "\x26";
   (* unicode 0x220d *) Symbol "\x27";
   (* unicode 0x2b*) Symbol "\x2b";
   (* unicode 0x39 *) Symbol "\x39";
   (* unicode 0x3e*) Symbol "\x3e";
   (* unicode 0x03a6*) Symbol "\x46";
   (* unicode 0x03d1*) Symbol "\x4a";
  (* unixode 0x0396 *) Symbol "\x5a";
  (* unicode 0x2234*) Symbol "\x5c";
  (* unicode 0x22a5 *) Symbol "\x5e";
   (* unicode 0x7b*) Symbol "\x7b";
   (* unicode 0x221e*) Symbol "\xa5";
   (* unicode 0x2666*) Symbol "\xa9";
  (* unicode 0xb1*) Symbol "\xb1";
  (* unicode 0x2202 *) Symbol "\xb6";
  (* unicode 0x2284 *) Symbol "\xcb";
  (* unicode 0x221a *) Symbol "\xd6";
   (* unicode 0x21d3*) Symbol "\xdf";
  (* unicode 0x25ca*) Symbol "\xe0";
  (* unicode 0x25b2 *) Zapf "\x73";
  (* unicode 0x274d *) Zapf "\x6d";
  (* unicode 0x2460 *) Zapf "\xac";
  (* unicode 0x2777 *) Zapf "\xb7";
]

