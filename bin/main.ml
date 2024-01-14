open Brc

let () =
  Riot.Logger.set_log_level (Some Debug);
  Riot.run @@ fun () ->
  let _pid = Riot.Logger.start () in
  ();
  Storage.make ();
  let filename = "measurements3.txt" in
  Files.read_file filename
