(* PROBLEM --
 * The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
 *
 * Find the last ten digits of the series, 1^(1) + 2^(2) + 3^(3) + ... +
 * 1000^(1000).
 *
 * SOLUTION --
 *)
open Big_int

let f n = power_int_positive_int n n

let sum ubd =
  let rec helper i sum =
  if i > ubd then sum
  else helper (succ i) (add_big_int sum (f i))
  in helper 1 zero_big_int

let str_series_1000 = string_of_big_int (sum 1000)

let len = String.length str_series_1000

let _ = print_endline(String.sub str_series_1000 (len-10) 10);;
