(* PROBLEM --
 * The number 3797 has an interesting property. Being prime itself, it is
 * possible to continuously remove digits from left to right, and remain prime
 * at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
 * left: 3797, 379, 37, and 3.
 *
 * Find the sum of the only eleven primes that are both truncatable from left to
 * right and right to left.
 *
 * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 *
 * SOLUTION --
 *)

(* There are 83 right-truncatable primes counting 2,3,5,7. *)
let r_trun_primes = [23; 29; 31; 37; 53; 59; 71; 73; 79; 233; 239; 293; 311;
313; 317; 373; 379; 593; 599; 719; 733; 739; 797; 2333; 2339; 2393; 2399;
2939; 3119; 3137; 3733; 3739; 3793; 3797; 5939; 7193; 7331; 7333; 7393; 23333;
23339; 23399; 23993; 29399; 31193; 31379; 37337; 37339; 37397; 59393; 59399;
71933; 73331; 73939; 233993; 239933; 293999; 373379; 373393; 593933; 593993;
719333; 739391; 739393; 739397; 739399; 2339933; 2399333; 2939999; 3733799;
5939333; 7393913; 7393931; 7393933; 23399339; 29399999; 37337999; 59393339;
73939133]

(* Drop everything that ends in 9 or 1 *)
let not_trivial = 
  let f n = (n mod 10 <> 1) && (n mod 10 <> 9) in
  List.filter f r_trun_primes

(* Drop everything that ends with a repeated digit *)
let not_embedded_eleven =
  let f n = (n mod 100) mod 11 <> 0 in
  List.filter f not_trivial

(* Drop all the 93s *)
let no_93s = List.filter (fun n -> (n mod 100) <> 93) not_embedded_eleven

(* 37337, 37397 and 7393913 are not l-trun *)
let solution = List.fold_left ( + ) (0-37337-37397-7393913) no_93s

let _ = Printf.printf "%d\n" solution;;
