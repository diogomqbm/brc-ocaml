type t = { name : string; value : float } [@@deriving show]

let make name value = { name; value }
let separator = ';'
let decimal = '.'

let is_number char =
  let code = Char.code char in
  code >= Char.code '0' && code <= Char.code '9' && code != Char.code '.'

let name t = t.name

(* Jacksonville;15.6Harbin;-2.7 *)
let from_line line =
  Riot.Logger.debug (fun f -> f "Processing line %s" line);
  let parseds : t list ref = ref [] in
  let name = ref "" in
  let value = ref "" in
  let one_last_char = ref false in
  String.iter
    (fun char ->
      match char with
      | ';' -> ()
      | '.' ->
          value := value.contents ^ String.make 1 char;
          one_last_char := true
      | '-' -> value := value.contents ^ String.make 1 char
      | char when is_number char && one_last_char.contents ->
          value := value.contents ^ String.make 1 char;
          let new_value = float_of_string value.contents in
          parseds := List.cons (make name.contents new_value) parseds.contents;
          name := "";
          value := "";
          one_last_char := false
      | char when is_number char -> value := value.contents ^ String.make 1 char
      | char -> name := name.contents ^ String.make 1 char)
    line;
  parseds.contents
