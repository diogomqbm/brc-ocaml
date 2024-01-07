open Riot.IO

[@@@warning "-8"]

let read_line reader buf_size =
  let str = "" in
  let rec _read_loop str =
    let buf_reader = Reader.Buffered.of_reader reader ~capacity:buf_size in
    let buf = Buffer.with_capacity 1 in
    let (Ok bytes_read) = Reader.read ~buf buf_reader in
    if Int.equal bytes_read 0 then None
    else
      let as_str = Buffer.to_string buf in
      if String.equal as_str "\n" then Some str else _read_loop (str ^ as_str)
  in
  _read_loop str

let rec read_loop reader buf_size =
  match read_line reader buf_size with
  | Some line ->
      Riot.Logger.debug (fun f -> f "Processing line %s" line);
      let _pid =
        Riot.spawn (fun () ->
            match Parsed.from_line line with
            | Some l -> Hashmap.add l
            | None -> Riot.Logger.debug (fun f -> f "Skipping line"))
      in
      ();
      read_loop reader buf_size
  | None -> Riot.Logger.info (fun f -> f "Reached EOF")

let read_file filename =
  let chunk_size = 1000 in
  let file_size =
    LargeFile.in_channel_length (open_in filename) |> Int64.to_int
  in
  let amount_of_chunks = Int.div file_size chunk_size in
  print_endline (string_of_int file_size);
  let rec loop n =
    if n > amount_of_chunks then ()
    else
      let _pid =
        Riot.spawn (fun () ->
            let seek = Int.mul n chunk_size in
            let file = Riot.File.open_read filename in
            let _f = Riot.File.seek file seek Unix.SEEK_SET in
            Riot.Logger.debug (fun f -> f "Reading file from %d" seek);
            let reader = Riot.File.to_reader file in
            read_loop reader chunk_size)
      in
      ();
      loop (n + 1)
  in
  loop 0
