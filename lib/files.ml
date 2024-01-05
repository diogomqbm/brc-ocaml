let read_file filename =
  let in_channel = open_in_bin filename in
  try
    while true do
      let line = input_line in_channel in
      Hashmap.add (Parsed.from_line line)
    done
  with
  | End_of_file ->
      print_endline "Finished reading file";
      close_in in_channel
  | e ->
      close_in_noerr in_channel;
      raise e
