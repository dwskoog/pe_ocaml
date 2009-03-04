(* PROBLEM --
 * A permutation is an ordered arrangement of objects. For example, 3124 is one
 * possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
 * are listed numerically or alphabetically, we call it lexicographic order. The
 * lexicographic permutations of 0, 1 and 2 are:
 *
 * 012   021   102   120   201   210
 *
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3,
 * 4, 5, 6, 7, 8 and 9?
 *
 * SOLUTION --
 * Originally, solved by hand: 2783915460
 *)

open Numbers

let rec nth_perm lst n =
  let len = List.length lst in
  if n > (factorial len) then failwith "No such permutation."
  else if len = 1 then lst
  else 
    let quotient = n / (factorial (len-1)) in
    let remainder = n mod (factorial (len-1)) in
    let hd = List.nth lst quotient in
    let tl = List.filter (function x -> x <> hd) lst in
    hd::(nth_perm tl remainder)

let rec print_perm perm =
  match perm with
  | [] -> ()
  | hd::tl -> print_int hd;print_perm tl


let main () =
  let perm = nth_perm [0;1;2;3;4;5;6;7;8;9] 999999 in
  print_perm perm;
  print_newline();;

main();;
