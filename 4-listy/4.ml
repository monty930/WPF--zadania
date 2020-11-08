let headn lst n =
  let rec pom ls it ans =
    if ls = [] || it = 0 then ans else 
    pom (tail ls) (it-1) ((head ls)::(ans))
  in
  rev (pom lst n [])
  ;;
let tailn lst n =
  let rec pom it ls =
    if ls = [] || it <= 0 then ls else
    pom (it-1) (tail ls)
  in
  pom (lenght lst - n) lst;;
