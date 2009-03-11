(* PROBLEM --
 * 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^(1000)?
 *
 * SOLUTION --
 *)

open Big_int
open Extensions
open String

let two_1000 = string_of_big_int (power_int_positive_int 2 1000)

let digits = List.map int_of_digit (explode two_1000)

let sum = List.fold_left ( + ) 0 digits

let _ = print_endline two_1000; print_int sum; print_newline();;
