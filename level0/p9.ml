(* PROBLEM --
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for
 * which,
 * a^(2) + b^(2) = c^(2)
 *
 * For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 *
 * SOLUTION --
 *)

open Numbers

let rec search a b =
  let c = 1000 - a - b in
  if c <= b then search (a+1) (a+2)
  else if is_py_triple a b c then (a,b,c,a*b*c)
  else search a (b+1)

let main () =
  let a,b,c,prod = search 1 2 in
  Printf.printf "%d, %d, %d :: %d" a b c prod;
  print_newline();;

main();;
