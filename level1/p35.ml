(* PROBLEM --
 * The number, 197, is called a circular prime because all rotations of the
 * digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
 *                                            37, 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 *
 * SOLUTION --
 * Unused reference material: 
 *   http://primes.utm.edu/glossary/page.php?sort=CircularPrime
 *)

open Extensions
open Primes
open Numbers

let rotate_list lst =
  match lst with
  | [] -> []
  | hd::tl -> tl@[hd]

let rotate_int n = 
  let n_string = string_of_int n in
  let n_list = List.map String.int_of_digit (String.explode n_string) in
  rotate_list n_list

let is_circular p =
  let rec helper rot_p =
    let rp = List.int_of_list rot_p in
    if rp = p then true
    else if is_prime rp then helper (rotate_list rot_p)
    else false
  in if is_prime p then helper (rotate_int p)
  else false

let circ_primes = List.filter is_circular (seq 1 1000000)

let solution = List.length circ_primes

let _ = print_int solution;print_newline();;
