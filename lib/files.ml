open Riot.IO

[@@@warning "-8"]

let read_lines ~filename ~offset () =
  let file = Riot.File.open_read filename in
  let _ = Riot.File.seek file offset Unix.SEEK_SET in
  let reader =
    Riot.File.to_reader file |> Reader.Buffered.of_reader ~capacity:(1_024 * 100)
  in

  let buf = Buffer.with_capacity 1 in
  let[@warning "-8"] rec read_line line =
    let (Ok bytes) = Reader.read ~buf reader in
    if bytes = 0 then line
    else
      let data = Buffer.to_string buf in
      let is_line = String.equal data "\n" in
      let line = if is_line then line else line ^ data in

      if is_line then Riot.Logger.info (fun f -> f "line: %S" line);

      if is_line then
        Option.iter (fun p -> Hashmap.add p) (Parsed.from_line line);

      if is_line then read_line "" else read_line line
  in
  let _ = read_line "" in
  ()

let read_file filename =
  let chunk_size = 1000 in
  let state = Riot.File.stat filename in
  let size = state.st_size in
  let chunks = size / chunk_size in
  let pids =
    List.init chunks (fun chunk ->
        let offset = chunk * chunk_size in
        Riot.spawn (read_lines ~filename ~offset))
  in

  Riot.wait_pids pids;
  Riot.Logger.info (fun f -> f "done");
  Riot.sleep 0.2
