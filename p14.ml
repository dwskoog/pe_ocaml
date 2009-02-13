(* PROBLEM --
 * The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following
 * sequence:
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1)
 * contains 10 terms. Although it has not been proved yet (Collatz Problem),
 * it is thought that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * SOLUTION --
 *)

open Numbers

let inputs = seq 1 999999

let stops = List.rev(List.rev_map collatz_stop inputs)

let build_length_tbl stop_list =
  let tbl = Hashtbl.create (List.length stop_list) in
  let rec helper lst =
    match lst with
    | [] -> tbl
    | (1,1,1)::tl -> Hashtbl.add tbl 1 1; helper tl
    | (n,stop,stop_time)::tl -> let len = Hashtbl.find tbl stop in
    begin
      Hashtbl.add tbl n (stop_time+len-1);
      helper tl
    end
  in helper stop_list

let biggest a b (n,t) = if b > t then (a,b) else (n,t)

let main () =
  let n,l = Hashtbl.fold biggest (build_length_tbl stops) (0,0) in
  Printf.printf "%d has stop length of %d\n" n l;;

main();;

