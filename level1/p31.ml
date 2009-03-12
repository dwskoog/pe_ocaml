(* PROBLEM --
 * In England the currency is made up of pound, £, and pence, p, and there are
 * eight coins in general circulation:
 *
 *   1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 *
 * It is possible to make £2 in the following way:
 *
 *   1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 *
 * How many different ways can £2 be made using any number of coins?
 *
 * SOLUTION --
 * a+2b+5c+10d+20e+50f+100g+200h=200
 *)
open Numbers

let coins = [200;100;50;20;10;5;2;1]

let rec str_pow elm n =
  if n = 0 then []
  else elm::(str_pow elm (n-1))

let rec print_list lst =
  match lst with
  | [] -> print_newline()
  | hd::[] -> print_int hd;print_newline()
  | hd::tl -> (Printf.printf "%d," hd);(print_list tl)

let rec count_combos coins value =
  match coins with
  | [] when value = 0 -> 1
  | [] -> 0
  | [1] when value > 0 -> 1
  | hd::tl when hd > value -> count_combos tl value
  | hd::tl ->
      let ubd = value/hd in
      let options = seq 0 ubd in
      let f x = count_combos tl (value-(x*hd)) in
      let sub_choices = List.map f options in
      (List.fold_left ( + ) 0 sub_choices)

let solution = count_combos coins 200

let _ = print_int solution; print_newline ();;
