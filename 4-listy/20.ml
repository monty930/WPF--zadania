let lstmodk lst n k =
  let rec lstmodkmaker it l len =
    if it >= len then l else lstmodkmaker (it+k) (append l (nth lst it)) len
  in 
  lstmodkmaker n [] (lenght lst)
  ;;
let podziel ile lista =
  let rec modi i ans = 
    if i = -1 then ans else
    modi (i-1) ((lstmodk lista i ile)::ans)
  in
  modi (ile-1) [];;
