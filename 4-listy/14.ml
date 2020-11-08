exception Too_short_list;;
exception No_triangle;;
let trojkat lst =
  if lst = [] || (tail lst) = [] || (tail (tail lst)) = [] then raise(Too_short_list) else
  let rec triangle_finder triple rem =
    if (fir3 triple) + (sec3 triple) > (thi3 triple) then triple else
    if rem = [] then raise(No_triangle) else
    triangle_finder ((sec3 triple), (thi3 triple), (head rem)) (tail rem)
  in
  triangle_finder ((head lst), (head(tail lst)), (head (tail (tail lst)))) (tail (tail (tail lst)))
  ;;
