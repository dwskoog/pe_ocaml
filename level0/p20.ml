(* PROBLEM --
 * n! means n × (n − 1) × ... × 3 × 2 × 1
 *
 * Find the sum of the digits in the number 100!
 *
 * SOLUTION --
 * Again, let's see what big_int can do.
 *)

open Extensions
open String
open List
open Big_int

let fact n = 
  let rec helper n f = 
    if eq_big_int zero_big_int n then f 
    else helper (pred_big_int n) (mult_big_int n f) 
  in 
    helper (big_int_of_int n) unit_big_int

let solution =
  let digits = map int_of_digit (explode (string_of_big_int (fact 100))) in
  fold_left ( + ) 0 digits

let main () =
  print_int solution;
  print_newline();;

main();;
