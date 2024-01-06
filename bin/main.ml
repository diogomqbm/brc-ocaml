open Brc

let () =
  Riot.Logger.set_log_level (Some Debug);
  Riot.run @@ fun () ->
  let _pid = Riot.Logger.start () in
  ();
  Hashmap.make 1000;
  let filename = "measurements2.txt" in
  let _content = Files.read_file filename in
  ();
  App.make ()
