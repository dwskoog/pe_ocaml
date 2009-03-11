(* PROBLEM --
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 *
 * SOLUTION --
 *)

open Primes

let _ =
  print_int(List.fold_left ( + ) 0 (sieve 2000000));
  print_newline();;
