open Riot

module Printer = struct
  let print (k, (v : Computed_line.t)) =
    let pid =
      spawn (fun () ->
          Logger.info (fun f -> f "\n%s=%.2f/%.2f/%.2f" k v.min v.mean v.max))
    in
    wait_pids [ pid ]
end

type t = { map : (string, Computed_line.t) Hashmap.t; mutable computed : int }
type Message.t += Add of Parsed.t | Finish

let pid = ref None

let rec loop state =
  (match receive () with
  | Add parsed ->
      let new_list =
        match Hashmap.get state.map parsed.name with
        | Some acc -> Computed_line.compute acc parsed.value
        | None -> Computed_line.from_value parsed.value
      in
      Hashmap.replace state.map parsed.name new_list;
      state.computed <- state.computed + 1;
      Logger.info (fun f -> f "Processed %d lines" state.computed)
  | Finish -> Hashmap.iter state.map Printer.print);
  loop state
[@@warning "-8"]

let add line =
  match pid.contents with Some pid -> send pid (Add line) | None -> ()

let finish () =
  match pid.contents with Some pid -> send pid Finish | None -> ()

let make () =
  pid :=
    Some
      (spawn_link (fun () ->
           let state = { map = Hashmap.create (); computed = 0 } in
           loop state))
