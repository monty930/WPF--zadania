let podziel lst =
  let rec pom rest now next seq ans =
    if rest = [] then ans else
    if now = head rest then pom (tail rest) (next) (next+1) [] ((append seq [head rest])::ans) else 
    pom (tail rest) (now) (next+1) (append seq [head rest]) ans
  in
  rev (pom lst 1 2 [] [])
  ;;
