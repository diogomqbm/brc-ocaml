open Riot

type t = string Stdlib.Queue.t
type Message.t += Push of string | Pop

[@@@warning "-8"]

let pid = ref None
let pop () = match pid.contents with Some pid -> send pid Pop | None -> ()

let push line =
  match pid.contents with Some pid -> send pid (Push line) | None -> ()

let rec loop q =
  pop ();
  (match receive () with
  | Push line -> Stdlib.Queue.push line q
  | Pop -> (
      match Stdlib.Queue.take_opt q with
      | Some line ->
          (* store line *)
          Riot.Logger.info (fun f -> f "Adding new line");
          Hashmap.add line
      | None -> ()));
  loop q

let make () =
  pid :=
    Some
      (spawn_link (fun () ->
           let q = Stdlib.Queue.create () in
           loop q))
