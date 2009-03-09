(* PROBLEM --
 * he decimal number, 585 = 1001001001_(2) (binary), is palindromic in both
 * bases.
 *
 * Find the sum of all numbers, less than one million, which are palindromic in
 * base 10 and base 2.
 *
 * (Please note that the palindromic number, in either base, may not include
 * leading zeros.)
 *
 * SOLUTON --
 *)
let rep_base b n =
  let rec bin_rep n = if n = 0 then [0] else (n mod b)::(bin_rep (n/b)) in
  let rep = List.rev(bin_rep n) in
  if n = 0 then [0]
  else if List.hd rep = 0 then List.tl rep
  else rep

let seq lbd ubd =
  let rec helper i s = if i<lbd then s else helper (i-1) (i::s)
  in helper ubd []

let is_odd n = n mod 2 = 1

let is_palindrome_base b n =
  let rep = Array.of_list (rep_base b n) in
  let rec helper left right =
    if left >= right then true
    else if rep.(left) = rep.(right) then helper (left+1) (right-1)
    else false
  in
  helper 0 ((Array.length rep)-1)

let is_good n = (is_palindrome_base 2 n) && (is_palindrome_base 10 n)

let good_nums = List.filter is_good (List.filter is_odd (seq 1 1000000))

let solution = List.fold_left ( + ) 0 good_nums

let _ = print_int solution;print_newline ();;

