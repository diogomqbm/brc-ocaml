open Riot
open IO

[@@@warning "-8"]

let read_lines ~filename ~offset ~limit () =
  let file = File.open_read filename in
  let _ = File.seek file ~off:offset in
  let reader = File.to_reader file in

  let buf = Bytes.with_capacity 1 in
  let read_until_now = ref 0 in
  let[@warning "-8"] rec read_line line =
    let (Ok bytes) = read ~buf reader in
    if bytes = 0 then ()
    else
      let data = Bytes.to_string buf in
      let is_line = String.equal data "\n" in
      read_until_now := read_until_now.contents + 1;

      let should_compute_line = read_until_now.contents >= limit && is_line in

      if should_compute_line then
        Option.iter (fun p -> Storage.add p) (Parsed.from_line line);

      if should_compute_line then ()
      else
        let line = if is_line then line else line ^ data in

        if is_line then
          Option.iter (fun p -> Storage.add p) (Parsed.from_line line);

        if is_line then read_line "" else read_line line
  in
  let _ = read_line "" in
  File.close file

let read_file filename =
  let chunk_size = 100 * 1024 in
  let state = File.stat filename in
  let size = state.st_size in
  let chunks = size / chunk_size in
  Logger.info (fun f -> f "Breaking into %d chunks" chunks);
  let chunks = if chunks = 0 then 1 else chunks in
  let pids =
    List.init chunks (fun chunk ->
        let is_last = chunk + 1 = chunks in
        let offset = chunk * chunk_size in
        let limit =
          if is_last then size else ((chunk + 1) * chunk_size) - offset
        in
        spawn (fun () ->
            try read_lines ~filename ~offset ~limit ()
            with Unix.Unix_error _ ->
              Logger.debug (fun f -> f "Unix error");
              sleep 1.0;
              read_lines ~filename ~offset ~limit ()))
  in

  wait_pids pids;
  Logger.info (fun f -> f "done");
  Storage.finish ()
