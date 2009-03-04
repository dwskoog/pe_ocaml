(* PROBLEM --
 * The Fibonacci sequence is defined by the recurrence relation:
 *
 *     F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.
 *
 * Hence the first 12 terms will be:
 *
 *     F_(1) = 1
 *     F_(2) = 1
 *     F_(3) = 2
 *     F_(4) = 3
 *     F_(5) = 5
 *     F_(6) = 8
 *     F_(7) = 13
 *     F_(8) = 21
 *     F_(9) = 34
 *     F_(10) = 55
 *     F_(11) = 89
 *     F_(12) = 144
 *
 * The 12th term, F_(12), is the first term to contain three digits.
 *
 * What is the first term in the Fibonacci sequence to contain 1000 digits?
 *
 * SOLUTION --
 *)

open Big_int;;

let one = unit_big_int;;
let lbd = power_int_positive_int 10 999;;

let ( + ) = add_big_int;;
let ( >= ) = ge_big_int;;

let rec find_fact lbd fn fn1 n =
  if fn >= lbd then n 
  else find_fact lbd fn1 (fn+fn1) (succ n);;

let solution = find_fact lbd one one 1;;

let main () =
  print_int solution;
  print_newline();;

main();;
