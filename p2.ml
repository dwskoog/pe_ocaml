(*
 * Problem:
 * Each new term in the Fibonacci sequence is generated by adding the previous
 * two terms. By starting with 1 and 2, the first 10 terms will be:
 *
 * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 *
 * Find the sum of all the even-valued terms in the sequence which do not
 * exceed four million.
 *
 * Solution:
 * Brute force
 *)

open Sequences
open Numbers

let even_bounded_fibs l = List.filter even (bounded_fib_seq l)

let main () =
  let fibs = even_bounded_fibs 4000000
  in
    print_int (List.fold_left ( + ) 0 fibs);
    print_newline();
    exit 0;;

main();;
