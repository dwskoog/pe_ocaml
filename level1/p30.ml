(* PROBLEM --
 * Surprisingly there are only three numbers that can be written as the sum of
 * fourth powers of their digits:
 *
 *   1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
 *   8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
 *   9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)
 *
 *   As 1 = 1^(4) is not a sum it is not included.
 *
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 *
 * Find the sum of all the numbers that can be written as the sum of fifth
 * powers of their digits.
 *
 * SOLUTION --
 * Reference material about Armstrong Numbers and PDIs:
 * http://www.mathews-archive.com/digit-related-numbers/pdi.html
 *
 * An n-digit number can sum to at most n*(9**5) for order 5.
 * 6 digits -> 354294
 * k-digits -> 10**k > k*(9**5), k>=7
 *)

open Extensions

let pow k n = truncate ((float n)**(float k))

let kord k n =
  let n_str = (string_of_int) n in
  let digits = List.map (String.int_of_digit) (String.explode n_str) in
  let kords = List.map (pow k) digits in
  List.fold_left ( + ) 0 kords

let is_5_order n = n = (kord 5 n)

let fifth_orders = List.filter is_5_order (Numbers.seq 2 354294)

let _ = print_int (List.fold_left ( + ) 0 fifth_orders); print_newline();;
