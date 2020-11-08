let noneq lst = 
  if lst = [] then 0 else
  let rec aux ls la cn =
    if ls = [] then cn else
    if (head ls) = la then aux (tail ls) la cn
    else aux (tail ls) (head ls) (cn+1)
  in 
  aux (tail lst) (head lst) 1
  ;;
