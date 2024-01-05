type t = { name : string; value : float }

let make name value = { name; value }

let from_line line =
  let items = String.split_on_char ';' line in
  let name = List.hd items in
  let value = float_of_string (List.nth items 1) in
  make name value
