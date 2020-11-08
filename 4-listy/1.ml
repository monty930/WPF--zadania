let nat n = 
  let rec pom i ans =
    if i = 0 then 0::ans else pom (i-1) (i::ans)
  in
  pom n [];;
