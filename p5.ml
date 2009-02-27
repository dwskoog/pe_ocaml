(* PROBLEM --
 * 2520 is the smallest number that can be divided by each of the numbers from 1
 * to 10 without any remainder.
 *
 * What is the smallest number that is evenly divisible by all of the numbers
 * from 1 to 20?
 *
 * SOLUTION --
 * lcm seq[1,20]
 *)

open Numbers

let reduce n m = if n // m then m / n else m

let rec sieve nums =
  match nums with
  | [] -> []
  | hd::tl -> hd::(sieve (List.map (reduce hd) tl))

let main () =
  let factors = sieve (seq 1 20) in
  print_int(List.fold_left ( * ) 1 factors);
  print_newline();;

main();;
