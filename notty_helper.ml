open Notty
open Notty_unix

let center img w h = I.vsnap h (I.hsnap w img)

let border (a: attr) (u: uchar) w h =
  I.vcat [
    I.uchar a u w 1;
    I.(uchar a u 1 (h-2) <|> I.void (w - 2) 1 <|> uchar a u 1 (h-2));
    I.uchar a u w 1
  ]