(* PROBLEM --
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 *
 * SOLUTION --
 *)

open Primes

let target = 600851475143

let _ = List.map (fun x -> print_int x; print_newline(); x) (prime_factors target);

