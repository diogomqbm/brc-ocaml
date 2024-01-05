open Riot

type t = {
  map : (string, Calculated.t) Hashtbl.t;
  total : int;
  mutable computed : int;
}

type Message.t += Add of Parsed.t | Finish

let pid = ref None

let rec loop state =
  (match receive () with
  | Add parsed ->
      let new_list =
        match Hashtbl.find_opt state.map parsed.name with
        | Some acc -> Calculated.compute acc parsed.value
        | None -> Calculated.from_value parsed.value
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
