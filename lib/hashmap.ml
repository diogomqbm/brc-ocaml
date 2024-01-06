open Riot

type t = {
  map : (string, Calculated.t) Hashmap.t;
  total : int;
  mutable computed : int;
}

type Message.t += Add of Parsed.t | Finish

let pid = ref None

let rec loop state =
  (match receive () with
  | Add parsed ->
      let new_list =
        match Hashmap.get state.map parsed.name with
        | Some acc -> Calculated.compute acc parsed.value
        | None -> Calculated.from_value parsed.value
      in
      Hashmap.replace state.map parsed.name new_list;
      state.computed <- state.computed + 1;
      Logger.info (fun f -> f "Processed %d lines" state.computed);
      if Int.equal state.computed (state.total - 1) then send (self ()) Finish
  | Finish -> Hashmap.iter state.map App.finish);
  loop state
[@@warning "-8"]

let add line =
  match pid.contents with Some pid -> send pid (Add line) | None -> ()

let make total =
  pid :=
    Some
      (spawn_link (fun () ->
           let state = { map = Hashmap.create 1000; total; computed = 0 } in
           loop state))
