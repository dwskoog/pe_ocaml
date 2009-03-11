(*
 * Problem:
 * If we list all the natural numbers below 10 that are multiples of 3 or 5,
 * we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 * Solution:
 * Basic application of the Principle of Inclusion/Exclusion
 *)

open Sequences

let main () =
  let sum3n  = List.fold_left ( + ) 0 (multiples_less_than 3 1000)
  and sum5n  = List.fold_left ( + ) 0 (multiples_less_than 5 1000)
  and sum15n = List.fold_left ( + ) 0 (multiples_less_than 15 1000) in
  print_int(sum3n + sum5n - sum15n);
  print_newline();
  exit 0;;

main();;
