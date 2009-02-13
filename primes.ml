open Numbers

let rec remove_all p n = 
  if p || n then remove_all p (n/p)
  else n

let prime_factors n =
  let factor_bound = truncate(sqrt(float n)) in
  let rec break n p ps =
    if p > factor_bound then if n = 1 then ps else n::ps
    else 
      let residue = remove_all p n in
      if residue < n then break residue (succ p) (p::ps)
      else break n (succ p) ps
  in break n 2 []

let sieve n =
  let bd = truncate(sqrt(float n)) in
  let candidates n = seq 2 n in
  let retain n m = m mod n <> 0 in
  let drop_multiples n l = List.filter (retain n) l in
  let rec helper nums primes =
    match nums with
    | []     -> primes
    | hd::tl -> if hd > bd then List.rev_append primes nums
                else helper (drop_multiples hd tl) (hd::primes)
  in
    helper (candidates n) []

let is_prime n =
  if n = 1 then false
  else if n < 4 then true
  else if 2 || n then false
  else if n < 9 then true
  else if 3 || n then false
  else
    let bd = truncate(sqrt(float n)) in
    let rec helper x =
      if x > bd then true
      else if x || n then false
      else if (x+2) || n then false
      else helper (x+6)
    in helper 5
