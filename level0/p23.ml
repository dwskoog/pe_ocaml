(* PROBLEM --
 * A perfect number is a number for which the sum of its proper divisors is
 * exactly equal to the number. For example, the sum of the proper divisors of
 * 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 *
 * A number whose proper divisors are less than the number is called deficient
 * and a number whose proper divisors exceed the number is called abundant.
 *
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
 * number that can be written as the sum of two abundant numbers is 24. By
 * mathematical analysis, it can be shown that all integers greater than 28123
 * can be written as the sum of two abundant numbers. However, this upper limit
 * cannot be reduced any further by analysis even though it is known that the
 * greatest number that cannot be expressed as the sum of two abundant numbers
 * is less than this limit.
 *
 * Find the sum of all the positive integers which cannot be written as the sum
 * of two abundant numbers.
 *
 * SOLUTION --
 *)

open Numbers
module Int_set = Set.Make(struct
                            type t = int
                            let compare = compare
                          end)

let abundant_nums = List.filter is_abundant (seq 12 28111)

let sum_of_two_abs =
  let rec helper n rem sums =
    match rem with
    | [] -> sums
    | hd::tl -> 
        let nsums = List.map (function x -> x + n) rem in
        let sums = List.fold_right Int_set.add ((2*n)::nsums) sums in
        helper hd tl sums
  in
  helper (List.hd abundant_nums) (List.tl abundant_nums) Int_set.empty

let not_sums =
  let candidates = List.fold_right Int_set.add (seq 1 28123) Int_set.empty in
  let sums = sum_of_two_abs in
  Int_set.diff candidates sums

let value = Int_set.fold ( + ) not_sums 0

open Printf
let main () =
  printf "#abuns = %d\n" (List.length abundant_nums);
  printf "#sums of abuns = %d\n" (Int_set.cardinal sum_of_two_abs);
  printf "#not sums = %d\n" (Int_set.cardinal not_sums);
  printf "Answer = %d\n" value;;

main();;
