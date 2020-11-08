let rec double lst =
  if lst = [] then [] else (head lst)::(head lst)::(double (tail lst));; 
