open Notty
open Notty_unix

(* [center img w h] returns a new Notty image with [img] placed in the center of
 * an image with width [w] and height [h] *)
let center img w h = I.vsnap h (I.hsnap w img)

(* [border a u w h] places a border in an image with [w] and height [h],
 * using character [u] and attributes [a] *)
let border a u w h =
  I.vcat [
    I.uchar a u w 1;
    I.(uchar a u 1 (h-2) <|> I.void (w - 2) 1 <|> uchar a u 1 (h-2));
    I.uchar a u w 1
  ]