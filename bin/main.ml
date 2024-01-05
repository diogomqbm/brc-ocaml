open Brc

let () =
  Riot.Logger.set_log_level (Some Debug);
  Riot.run @@ fun () ->
  App.pid := Some (Riot.self ());
  let filename = "measurements2.txt" in
  Hashmap.make 1000;
  Files.read_file filename;
  App.loop ()
