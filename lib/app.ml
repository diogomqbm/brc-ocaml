open Riot

module Printer = struct
  let print k (v : Calculated.t) =
    let _pid =
      spawn (fun () ->
          Logger.info (fun f -> f "\n%s=%.2f/%.2f/%.2f" k v.min v.mean v.max))
    in
    ()
end

type Message.t += Print of string * Calculated.t

let rec loop () =
  (match receive () with Print (name, item) -> Printer.print name item);
  loop ()
[@@warning "-8"]

let pid = ref None

let finish (k, v) =
  match pid.contents with
  | Some pid -> send pid (Print (k, v))
  | None -> print_endline "Oops!"

let make () =
  pid := Some (Riot.self ());
  loop ()
