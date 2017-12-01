open Notty
open Notty_unix

let center img w h =
  let top_margin = (h - I.height img) / 2 in
  let left_margin = (w - I.width img) / 2 in
  I.pad ~t:top_margin ~l:left_margin img