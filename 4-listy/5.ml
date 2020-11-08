let rozwiniecie lista =
  let rec rozwiniator pozostalosc odpowiedz =
    if pozostalosc = [] then odpowiedz else
    rozwiniator (tail pozostalosc) (append odpowiedz (repeat (fir (head pozostalosc)) (sec (head pozostalosc))))
  in
  rozwiniator lista []
  ;; 

