(* PROBLEM --
 * f p is the perimeter of a right angle triangle with integral length sides,
 * {a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p ≤ 1000, is the number of solutions maximised?
 *
 * SOLUTION --
 * go go pythagorean triples.
 *)

open Numbers

let ps = (seq 1 1000)

let is_solution p a b = 
  let c = p - a - b in is_py_triple a b c

let b_range p a = if p-a-1 >= a then seq a (p-a-1) else []

let a_range p = seq 1 (p-2)

let count p =
  let helper a = List.length (List.filter (is_solution p a) (b_range p a)) in
  List.fold_left ( + ) 0 (List.map helper (a_range p))

let counts = List.map count ps;;

let solution,count = 
  let f (p,pc) x xc = if xc > pc then (x,xc) else (p,pc) in
  List.fold_left2 f (0,0) ps counts

let _ = Printf.printf "p = %d, count = %d\n" solution count;;
