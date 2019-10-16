type font = [ `Zapf of string | `Symbol of string ]

type t = {
  uchar : Uchar.t;
  pdf : font;
}

let default =
  { uchar = Uchar.of_int 0x20;
    pdf = `Symbol "\x20" }

let printable_symbols = [
  { uchar = Uchar.of_int 0x23;   pdf = `Symbol "\x23" };
  { uchar = Uchar.of_int 0x25;   pdf = `Symbol "\x25" };
  { uchar = Uchar.of_int 0x220d; pdf = `Symbol "\x27" };
  { uchar = Uchar.of_int 0x2b;   pdf = `Symbol "\x2b" };
  { uchar = Uchar.of_int 0x39;   pdf = `Symbol "\x39" };
  { uchar = Uchar.of_int 0x3e;   pdf = `Symbol "\x3e" };
  { uchar = Uchar.of_int 0x03a6; pdf = `Symbol "\x46" };
  { uchar = Uchar.of_int 0x03d1; pdf = `Symbol "\x4a" };
  { uchar = Uchar.of_int 0x0396; pdf = `Symbol "\x5a" };
  { uchar = Uchar.of_int 0x2234; pdf = `Symbol "\x5c" };
  { uchar = Uchar.of_int 0x22a5; pdf = `Symbol "\x5e" };
  { uchar = Uchar.of_int 0x7b;   pdf = `Symbol "\x7b" };
  { uchar = Uchar.of_int 0x221e; pdf = `Symbol "\xa5" };
  { uchar = Uchar.of_int 0x2666; pdf = `Symbol "\xa9" };
  { uchar = Uchar.of_int 0xb1;   pdf = `Symbol "\xb1" };
  { uchar = Uchar.of_int 0x2202; pdf = `Symbol "\xb6" };
  { uchar = Uchar.of_int 0x21d3; pdf = `Symbol "\xdf" };
  { uchar = Uchar.of_int 0x2284; pdf = `Symbol "\xcb" };
  { uchar = Uchar.of_int 0x221a; pdf = `Symbol "\xd6" };
  { uchar = Uchar.of_int 0x21d3; pdf = `Symbol "\xdf" };
  { uchar = Uchar.of_int 0x25ca; pdf = `Symbol "\xe0" };
  { uchar = Uchar.of_int 0x25b2; pdf = `Zapf "\x73" };
  { uchar = Uchar.of_int 0x274d; pdf = `Zapf "\x6d" };
  { uchar = Uchar.of_int 0x2460; pdf = `Zapf "\xac" };
  { uchar = Uchar.of_int 0x2777; pdf = `Zapf "\xb7" };
]

