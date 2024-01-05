open Riot

module Parsed = struct
  type t = { name : string; value : float }

  let make name value = { name; value }
end

type t = {
  map : (string, float list) Hashtbl.t;
  total : int;
  mutable computed : int;
}

type Message.t += Add of string | Finish

let pid = ref None

let parse line =
  let items = String.split_on_char ';' line in
  let name = List.hd items in
  let value = float_of_string (List.nth items 1) in
  Parsed.make name value

let rec loop state =
  (match receive () with
  | Add line ->
      let parsed = parse line in
      let new_list =
        match Hashtbl.find_opt state.map parsed.name with
        | Some parsed_list -> parsed.value :: parsed_list
        | None -> [ parsed.value ]
      in
      Hashtbl.replace state.map parsed.name new_list;
      state.computed <- state.computed + 1;
      if Int.equal state.computed state.total then send (self ()) Finish
  | Finish -> Hashtbl.iter App.finish state.map);
  loop state
[@@warning "-8"]

let add line =
  match pid.contents with Some pid -> send pid (Add line) | None -> ()

let make total =
  pid :=
    Some
      (spawn_link (fun () ->
           let state = { map = Hashtbl.create 1000; total; computed = 0 } in
           loop state))
