(* PROBLEM --
 * An irrational decimal fraction is created by concatenating the positive
 * integers:
 *
 * 0.123456789101112131415161718192021...
 *              ^
 * It can be seen that the 12^(th) digit of the fractional part is 1.
 *
 * If d_(n) represents the n^(th) digit of the fractional part, find the value
 * of the following expression.
 *
 * d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) × d_(1000000)
 *
 * SOLUTION --
 *   1-  9:    9 digits
 *  10- 99:  180 digits
 * 100-999: 2700 digits
 *)

open Extensions

let p40_seq initial =
  let next_int = ref initial in
  let buf = ref [] in
  let rec next i =
    if !buf = [] then (
      buf := List.list_of_int !next_int;
      next_int := succ !next_int
    );
    match !buf with
    | h :: t -> (buf := t; Some h)
    | [] -> None in
  Stream.from next

let sequence = (p40_seq 1)

let indices = [1;10;100;1000;10000;100000;1000000]

let find seq ns =
  let max = List.hd (List.rev ns) in
  let rec helper step ds =
    let d = Stream.next seq in
    if step > max then ds
    else if List.mem step ns then
      helper (step + 1) (d::ds)
    else
      helper (step + 1) ds
  in
  helper 1 []

let ds = find sequence indices

let solution = List.fold_left ( * ) 1 ds

let _ = print_int solution; print_newline();;
