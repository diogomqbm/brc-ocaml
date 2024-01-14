type t = { sum : float; min : float; max : float; mean : float; count : int }

let make ~min ~mean ~max ~sum ~count = { sum; min; max; mean; count }

exception Could_find_values

let compute acc x =
  let new_sum = acc.sum +. x in
  let new_min = if x < acc.min then x else acc.min in
  let new_max = if x > acc.max then x else acc.max in
  let new_count = acc.count + 1 in
  let new_mean = new_sum /. float_of_int new_count in
  make ~min:new_min ~sum:new_sum ~max:new_max ~count:new_count ~mean:new_mean

let from_value x = make ~min:x ~max:x ~sum:x ~mean:x ~count:1
