let zera_silni n =
  let rec pom i ans =
    if i = 0 then ans else pom (i/5) (ans + i/5)
  in
  pom n 0
  ;;
