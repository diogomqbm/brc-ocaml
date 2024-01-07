type t = { name : string; value : float } [@@deriving show]

let make name value = { name; value }
let separator = ';'
let decimal = '.'

let is_number char =
  let code = Char.code char in
  code >= Char.code '0' && code <= Char.code '9' && code != Char.code '.'

let name t = t.name

exception Incomplete_line

let from_line line =
  Riot.Logger.debug (fun f -> f "Processing line %s" line);
  if String.length line <= 5 then None
  else
    let items = String.split_on_char ';' line in
    let name = List.hd items in
    let value = List.nth items 1 in
    Some (make name (float_of_string value))
