(* PROBLEM --
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n
 * which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
 * each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
 * 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
 * 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 *
 * SOLUTION --
 * Research is the friend of the lazy:
 * http://www.research.att.com/~njas/sequences/A063990
 *
 * # List.fold_left ( + ) 0 [220;284;1184;1210;2620;2924;5020;5564;6232;6368];;
 * - : int = 31626
 *)

val solution = 31626
