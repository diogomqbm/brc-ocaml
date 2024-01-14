open Riot
open IO

[@@@warning "-8"]

let read_lines ~filename ~offset ~limit () =
  let file = Riot.File.open_read filename in
  let _ = File.seek file ~off:offset in
  let reader = Riot.File.to_reader file in

  let buf = Buffer.with_capacity 1 |> Buffer.to_bytes in
  let read_until_now = ref 0 in
  let[@warning "-8"] rec read_line line =
    let (Ok bytes) = Riot.IO.read ~buf reader in
    if bytes = 0 then line
    else
      let data = Bytes.to_string buf in
      let is_line = String.equal data "\n" in
      read_until_now := read_until_now.contents + 1;
      if read_until_now.contents = limit then
        Riot.Logger.info (fun f -> f "Reached limit: %d, %s" limit line);
      if read_until_now.contents = limit then line
      else
        let line = if is_line then line else line ^ data in

        if is_line then Riot.Logger.info (fun f -> f "line: %S" line);

        if is_line then
          Option.iter (fun p -> Storage.add p) (Parsed.from_line line);

        if is_line then read_line "" else read_line line
  in
  let _ = read_line "" in
  ()

let read_file filename =
  let chunk_size = 10 in
  let state = Riot.File.stat filename in
  let size = state.st_size in
  let chunks = size / chunk_size in
  let pids =
    List.init chunks (fun chunk ->
        let is_last = chunk + 1 = chunks in
        let offset = chunk * chunk_size in
        let limit =
          if is_last then size else ((chunk + 1) * chunk_size) - offset
        in
        Riot.spawn (fun () ->
            try read_lines ~filename ~offset ~limit ()
            with Unix.Unix_error _ ->
              Riot.Logger.debug (fun f -> f "Found the Unix error");
              Riot.sleep 1.0;
              read_lines ~filename ~offset ~limit ()))
  in

  Riot.wait_pids pids;
  Riot.Logger.info (fun f -> f "done");
  Storage.finish ();
  Riot.sleep 0.2
