open Brc

let () =
  Riot.Logger.set_log_level (Some Debug);
  Riot.run @@ fun () ->
  let _pid = Riot.Logger.start () in
  ();
  Hashmap.make ();
  let filename = "measurements.txt" in
  Files.read_file filename
