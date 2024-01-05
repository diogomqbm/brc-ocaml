open Riot

module Printer = struct
  let print k v =
    let _pid =
      spawn_link (fun () ->
          let computed = Calculated.from_values v in
          Printf.printf "\n%s=%.2f/%.2f/%.2f" k computed.min computed.mean
            computed.max)
    in
    ()
end

type Message.t += Print of string * float list

let rec loop () =
  (match receive () with Print (name, item) -> Printer.print name item);
  loop ()
[@@warning "-8"]

let pid = ref None

let finish k v =
  match pid.contents with Some pid -> send pid (Print (k, v)) | None -> ()
