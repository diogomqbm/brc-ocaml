let rec read_line ic str =
  let char = input_char ic in
  if Char.equal char '\n' then print_endline str
  else read_line ic (str ^ String.make 1 char)

let read_loop ic end_pos =
  try
    while LargeFile.pos_in ic < end_pos do
      let line = input_line ic in
      let parseds = Parsed.from_line line in
      List.iter (fun p -> Hashmap.add p) parseds
    done
  with
  | End_of_file -> close_in ic
  | e -> raise e

let read_file filename =
  let chunk_size = 10000L in
  let file_size = LargeFile.in_channel_length (open_in filename) in
  let amount_of_chunks = Int64.div file_size chunk_size in
  let rec loop n =
    if n >= amount_of_chunks then ()
    else
      let _pid =
        Riot.spawn (fun () ->
            let seek = Int64.mul n chunk_size in
            let ic = open_in filename in
            let end_pos = Int64.mul (Int64.add n 1L) chunk_size in
            LargeFile.seek_in ic (Int64.mul n seek);
            Riot.Logger.debug (fun f ->
                f "Starting from %d to %d"
                  (Int64.to_int (LargeFile.pos_in ic))
                  (Int64.to_int end_pos));
            read_loop ic end_pos)
      in
      ();
      loop (Int64.add n 1L)
  in
  loop 0L
