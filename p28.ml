(* PROBLEM --
 * Starting with the number 1 and moving to the right in a clockwise
 * direction a 5 by 5 spiral is formed as follows:
 *
 * 21 22 23 24 25
 * 20  7  8  9 10
 * 19  6  1  2 11
 * 18  5  4  3 12
 * 17 16 15 14 13
 *
 * It can be verified that the sum of both diagonals is 101.
 *
 * What is the sum of both diagonals in a 1001 by 1001 spiral formed in the
 * same way?
 *
 * SOLUTION --
 *)

let rec spiral n = if n = 1 then 1 else (4*n*n)-(6*n)+6+(spiral (n-2));;

let _ = print_int (spiral 1001);print_newline();;
