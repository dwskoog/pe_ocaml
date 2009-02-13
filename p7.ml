(* PROBLEM --
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
 * that the 6^(th) prime is 13.
 *
 * What is the 10001^(st) prime number?
 *
 * SOLUTION --
 * Sieve of Eratosthenes
 *)

open Numbers
open Primes

let main () =
  let primes = sieve 200000
  in
    print_int(List.nth primes 10000);
    print_newline();;

main();;
