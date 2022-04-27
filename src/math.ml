(* a^n mod m *)
let mod_expt a n m =
  let rec aux b n m r =
    if n = 0 then r
    else if n land 1 = 0 then aux (b * b mod m) (n asr 1) m r
    else aux (b * b mod m) (n asr 1) m (r * b mod m) in
  aux a n m 1
