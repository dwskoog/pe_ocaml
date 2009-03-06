(* PROBLEM --
 * Euler published the remarkable quadratic formula:
 *
 *   n² + n + 41
 *
 * It turns out that the formula will produce 40 primes for the consecutive
 * values n = 0 to 39. However, when n = 40, 40^(2) + 40 + 41 = 40(40 + 1) +
 * 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
 * divisible by 41.
 *
 * Using computers, the incredible formula  n² − 79n + 1601 was discovered,
 * which produces 80 primes for the consecutive values n = 0 to 79. The
 * product of the coefficients, −79 and 1601, is −126479.
 *
 * Considering quadratics of the form:
 *
 *   n² + an + b, where |a| < 1000 and |b| < 1000
 *
 *    where |n| is the modulus/absolute value of n e.g. |11| = 11 and |-4| = 4
 *
 * Find the product of the coefficients, a and b, for the quadratic expression
 * that produces the maximum number of primes for consecutive values of n,
 * starting with n = 0.
 *
 * SOLUTION --
 *)
open Extensions
open Numbers
open Primes

let p a b n = n*n+a*n+b

let p_len f =
  let rec helper i = 
    if is_prime(f i) then helper (i+1)
    else (i-1)
  in helper 0

(* Since (p a b 0) must be prime, b must be prime. *)
let possible_bs = List.filter is_prime (seq 1 1000)

let possible_as = List.map map_N_to_Z (seq 0 2000)

let possible_functions = List.flatten (List.cross possible_as possible_bs)

(* If (p a b) is prime-generating on 0<=n<=m then (p a b)(n-m) is
 * prime-generating on 0<=n<=2m
 *)
let transform a b m = (a-2*m),(m*m-a*m+b),2*m

let len_map = Hashtbl.create (List.length possible_functions)

let rec compute_length remfs =
  match remfs with
  | [] -> len_map
  | (a,b)::tl ->
      if Hashtbl.mem len_map (a,b) then compute_length tl else
      let m = p_len (p a b) in
      let ta, tb, tm = transform a b m in begin
      Hashtbl.add len_map (a,b) m;
      if abs(ta) < 1000 && abs(tb) < 1000 then Hashtbl.add len_map (ta,tb) tm;
      compute_length tl
      end

let solution =
  let _ = compute_length possible_functions in
  let f = (fun k d (k2,d2) -> if d>d2 then (k,d) else (k2,d2)) in
  let (a,b),m = (Hashtbl.fold f len_map ((0,0),0)) in
  a*b

let main () =
  print_int solution;
  print_newline ();;

main();;
