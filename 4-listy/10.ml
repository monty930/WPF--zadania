exception Empty_list;;
let minimax lst = 
  if lst = [] then raise(Empty_list) else
  let rec finder min max rem =
    if rem = [] then (min, max) else
    if (head rem) < min then finder (head rem) max (tail rem)
    else if (head rem) > max then finder min (head rem) (tail rem)
    else finder min max (tail rem)
  in
  finder (head lst) (head lst) (tail lst)
  ;;

