open Notty

let square = "\xe2\x96\xaa"

let rec sierp n =
  if n > 1 then
    let ss = sierp (pred n) in I.(ss <-> (ss <|> ss))
  else I.(string A.(fg magenta) square |> hpad 1 0)

let start () = sierp 8 |> Notty_unix.output_image

let () = start ()