(* PROBLEM --
 * Starting in the top left corner of a 2×2 grid, there are 6 routes (without
 * backtracking) to the bottom right corner.
 *
 * How many routes are there through a 20×20 grid?
 *
 * SOLUTION --
 * An NxN grid requires N "down" steps and N "right" step.
 *
 * C(2N,N) = (2N)!/(N! * N!) = PROD[N+1...2N] / PROD[1..N]
 *)

open Numbers

let num_facts = (seq 21 40)

let denom_facts = (seq 1 20)

let rec reduce_list_by n lst =
  match lst with
  | [] -> false,[]
  | hd::tl when n || hd -> true,((hd/n)::tl)
  | hd::tl ->
      let did_reduce,new_tl = reduce_list_by n tl in
      if did_reduce then true,hd::new_tl else false,hd::tl

let reduce_by reducee reducer =
  let rec helper remaining unproc residues =
    match unproc with
    | [] -> remaining,residues
    | hd::tl ->
        let reduced,new_remaining = reduce_list_by hd remaining in
        helper new_remaining tl (if reduced then residues else (hd::residues))
  in
  let remains,residues = helper reducee reducer [] in
  remains,List.rev(residues)

let main () =
  let nums,denoms = reduce_by num_facts denom_facts in
  let num = (List.fold_left ( * ) 1 nums) in
  let denom = (List.fold_left ( * ) 1 denoms) in
  print_int(num/denom);
  print_newline();;

main();;
