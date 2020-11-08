exception Empty_list;;
let rec last lst = 
  if lst = [] then raise(Empty_list) else
  if (tail lst = []) then (head lst) else last (tail lst);;
