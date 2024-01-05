type t = { sum : float; min : float; max : float; mean : float }

let make ~min ~mean ~max ~sum = { sum; min; max; mean }

exception Could_find_values

let compute acc x =
  let new_sum = acc.sum +. x in
  let new_min = if x < acc.min then x else acc.min in
  let new_max = if x > acc.max then x else acc.max in
  make ~min:new_min ~sum:new_sum ~max:new_max ~mean:0.0

let from_values values =
  match values with
  | [] -> raise Could_find_values
  | hd :: tl ->
      let acc =
        List.fold_left compute (make ~min:hd ~max:hd ~mean:hd ~sum:hd) tl
      in
      let mean = acc.sum /. float_of_int (List.length values) in
      make ~min:acc.min ~sum:acc.sum ~max:acc.max ~mean
