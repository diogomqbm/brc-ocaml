open Riot

type t = { mutable total : int; mutable computed : int }
type Message.t += AddComputed | Add

let pid = ref None

let rec loop t =
  (match receive () with
  | Add -> t.total <- t.total + 1
  | AddComputed ->
      t.computed <- t.computed + 1;
      if t.computed = t.total then print_endline (string_of_int t.computed);
      print_endline (string_of_int t.total)
  | _ -> ());
  loop t

let add_computed () =
  match pid.contents with Some pid -> send pid AddComputed | None -> ()

let make total =
  pid :=
    Some
      (spawn_link (fun () ->
           let t = { total; computed = 0 } in
           loop t))
