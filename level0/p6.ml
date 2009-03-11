(* PROBLEM --
 * The sum of the squares of the first ten natural numbers is,
 * 1^(2) + 2^(2) + ... + 10^(2) = 385
 *
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
 *
 * Hence the difference between the sum of the squares of the first ten natural
 * numbers and the square of the sum is 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred
 * natural numbers and the square of the sum.
 *
 * SOLUTION --
 *)

open Numbers

let sq n = n*n

let sum_of_squares nums = 
  let sq_nums = List.map sq nums
  in
  List.fold_left ( + ) 0 sq_nums

let square_of_sum nums =
  let sum_nums = List.fold_left ( + ) 0 nums
  in
  sq sum_nums

let n_100 = (seq 1 100)

let main () =
  print_int(square_of_sum(n_100) - sum_of_squares(n_100));
  print_newline();;

main();;

