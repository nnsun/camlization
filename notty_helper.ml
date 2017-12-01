open Notty
open Notty_unix

let center img w h = I.vsnap h (I.hsnap w img)