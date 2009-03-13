(* PROBLEM --
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *
 *  192 × 1 = 192
 *  192 × 2 = 384
 *  192 × 3 = 576
 *
 * By concatenating each product we get the 1 to 9 pandigital, 192384576. We
 * will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 * and 5, giving the pandigital, 918273645, which is the concatenated product
 * of 9 and (1,2,3,4,5).
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 * the concatenated product of an integer with (1,2, ... , n) where n > 1?
 *
 * SOLUTION --
 *)

open Extensions
open Numbers

let is_pandigital str =
  let digits = ['1';'2';'3';'4';'5';'6';'7';'8';'9'] in
  let used_digits = List.sort compare (String.explode str) in
  digits = used_digits

let smallest_non_repeating_n_digit n = List.int_of_list (seq 1 n)

let largest_non_repeating_n_digit n =
  List.int_of_list (List.rev (seq (10-n) 9))

let complement_bound lst = 
  let n = List.length lst in
  let lb = smallest_non_repeating_n_digit (9/n) in
  let ub = largest_non_repeating_n_digit (9/n) in
  (lb,ub)

let concat_prod lst n =
  let prods = List.map string_of_int (List.map (( * ) n) lst) in
  let cp = List.fold_left ( ^ ) "" prods in
  if is_pandigital cp then int_of_string cp else 0

let search lst =
  let first,last = complement_bound lst in
  let candidates = seq first last in
  let cps = List.map (concat_prod lst) candidates in
  if cps <> [] then
    List.hd (List.sort (fun x y -> compare y x) cps)
  else 0

let largest_per_n = 
  let two_to_nine = seq 2 9 in
  let seqs = List.map (seq 1) two_to_nine in
  List.map search seqs

let solution = List.hd (List.sort (fun x y -> compare y x) largest_per_n)

let _ = print_int solution;print_newline();
