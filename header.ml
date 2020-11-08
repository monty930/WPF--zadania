let fir x = match x with (a,_) -> a;;
let sec x = match x with (_,a) -> a;;
let fir3 x = match x with (a,_,_) -> a;;
let sec3 x = match x with (_,a,_) -> a;;
let thi3 x = match x with (_,_,a) -> a;;
let tail x = match x with _::a -> a;;
let head x = match x with a::_ -> a;;
let abs a = if a < 0 then (-a) else a;;
let rec lenght lst = if lst = [] then 0 else 1+lenght (tail lst);;
let rec nth l n =
  if l = [] then [] else
  if n = 0 then [head l] else nth (tail l) (n-1);;
let rev l = 
  let rec pom l w =
    if l = [] then w
    else pom (tail l) ((head l)::w)
  in pom l [];;
let rec append l1 l2 = if l1 = [] then l2 else (head l1)::(append (tail l1) l2);;
let rec repeat a n = if n = 0 then [] else (a::(repeat a (n-1)));;
let rec dec_to_bin x = if x = 0 then 0 else (x mod 2) + 10*(dec_to_bin (x/2));;
