type int64_list = int64 list [@@deriving show]

let find_chunk_offsets filename chunk_size =
  let ic = open_in filename in
  let offsets : int64 list ref = ref [ 0L ] in
  try
    while true do
      for _ = 1 to chunk_size do
        ignore (input_line ic)
      done;
      offsets := LargeFile.pos_in ic :: !offsets
    done;
    !offsets
  with End_of_file ->
    close_in ic;
    List.rev !offsets

let read_chunk filename start_offset end_offset =
  let ic = open_in_bin filename in
  LargeFile.seek_in ic start_offset;
  let rec read_lines acc =
    if LargeFile.pos_in ic < end_offset then read_lines (input_line ic :: acc)
    else acc
  in
  let lines = read_lines [] in
  close_in ic;
  List.rev lines

let read_file filename =
  let chunk_size = 1_000_000 in
  let offsets = find_chunk_offsets filename chunk_size in
  print_endline (show_int64_list offsets);
  let chunks =
    List.map2
      (fun start end_ -> (start, end_))
      offsets
      (List.tl offsets @ [ LargeFile.in_channel_length (open_in_bin filename) ])
  in
  List.iter
    (fun (start, end_) ->
      let _pid =
        Riot.spawn_link (fun () ->
            let lines = read_chunk filename start end_ in
            List.iter (fun line -> Queue.push line) lines)
      in
      ())
    chunks
