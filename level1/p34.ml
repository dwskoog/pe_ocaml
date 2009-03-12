(* PROBLEM --
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of
 * their digits.
 *
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 *
 * SOLUTION --
 * Factorions are always smaller than 7*9!.
 *)
open Numbers

let factorionize n =
  let rec helper sum n =
    if n = 0 then sum
    else helper (sum + (factorial (n mod 10))) (n/10)
  in helper 0 n

let is_factorion n = n = (factorionize n)

let find_bounded_factorions ubd =
  let rec helper fs i =
    if i > ubd then fs
    else if is_factorion i then helper (i::fs) (i+1)
    else helper fs (i+1)
  in helper [] 10

let factorions =
  let ubd = 7 * (factorial 9) in
  find_bounded_factorions ubd

let solution = List.fold_left ( + ) 0 factorions

let _ = print_int solution; print_newline ();;
