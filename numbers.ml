let even n = n mod 2 = 0

let odd n = n mod 2 = 1

let divides p n = n mod p = 0

let ( || ) p n = divides p n

let seq lbd ubd =
  let rec helper n ns = 
    if n >= lbd then helper (n-1) (n::ns) else ns
  in helper ubd []

let is_py_triple a b c = a*a + b*b = c*c

let tri_num n = (n*(n+1))/2

let collatz_succ n = if 2 || n then n/2 else 3*n+1

let collatz_seq n =
  let rec helper n c_seq =
    if n = 1 then 1::c_seq
    else helper (collatz_succ n) (n::c_seq)
  in helper n []

let collatz_stop n =
  let rec helper curr len =
    if n = 1 then (1,1,1)
    else if curr = n then failwith ("Self succeeding:" ^ (string_of_int n))
    else if curr < n then (n, curr, len + 1)
    else helper (collatz_succ curr) (len+1)
  in helper (collatz_succ n) 1

let factorial n =
  let rec helper curr prod =
    if curr = 1 then prod
    else helper (pred curr) (curr*prod)
  in helper n 1
