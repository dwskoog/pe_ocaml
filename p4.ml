(* PROBLEM --
 * A palindromic number reads the same both ways. The largest palindrome made
 * from the product of two 2-digit numbers is 9009 = 91 × 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 *
 * SOLUTION --
 *)

let rev_cmp x y = - compare x y

open Extensions
open String

let is_num_pal n = is_palindrome(string_of_int n)

let up_tri_enum f bd =
  let rec helper n m residues =
    let new_res = if f(n*m) then (n*m)::residues else residues
    in
    if m = 100 && n > 100 then helper (n-1) (n-1) new_res
    else if m = 100 then new_res
    else helper n (m-1) new_res
  in
    helper bd bd []

let search =
  let pals = (up_tri_enum is_num_pal 999)
  in List.hd(List.sort rev_cmp pals)

let main () = 
  print_int (search);
  print_newline();;

main();;
