(* PROBLEM --
 * A unit fraction contains 1 in the numerator. The decimal representation of
 * the unit fractions with denominators 2 to 10 are given:
 *
 *      ^(1)/_(2)  =  0.5
 *      ^(1)/_(3)  =  0.(3)
 *      ^(1)/_(4)  =  0.25
 *      ^(1)/_(5)  =  0.2
 *      ^(1)/_(6)  =  0.1(6)
 *      ^(1)/_(7)  =  0.(142857)
 *      ^(1)/_(8)  =  0.125
 *      ^(1)/_(9)  =  0.(1)
 *      ^(1)/_(10) =  0.1
 *
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
 * seen that ^(1)/_(7) has a 6-digit recurring cycle.
 *
 * Find the value of d < 1000 for which ^(1)/_(d) contains the longest 
 * recurring cycle in its decimal fraction part.
 *
 * SOLUTION --
 * Terminating representations occur for denominators composed of 2s and 5s in
 * decimal.
 *
 * 1/d has a repeating pattern of fewer than d digits and the pattern starts in
 * the first d digits following the decimal point.
 *
 * Dumb, brutal and slow approach follows:

open Extensions
open Primes
open Numbers
open Num
open Str

let is_terminating d = (remove_all 2 (remove_all 5 d)) = 1

let is_repeating d = not (is_terminating d)

let dec_rep d = approx_num_fix (2*d) ((num_of_int 1) // (num_of_int d))

let mantissa d =
  let rep = dec_rep d in
  let len = String.length rep in
  String.slice rep 3 (len - 1)

let match_forward r s start =
  try search_forward r s start
  with Not_found -> -1

let pattern_search mant d =
  let rec helper fpos len =
    if len >= d then ""
    else begin
      let pat = "\\(" ^ (String.sub mant fpos len) ^ "\\)+\\(.*\\)" in
      let r = regexp pat in
      if not(string_match r mant fpos) then
        if fpos + len = d then helper 0 (len+1)
        else helper (fpos+1) len
      else begin
        let cyc = matched_group 1 mant in
        let rem = matched_group 2 mant in
        if String.length cyc >= String.length rem then cyc
        else if fpos + len = d then helper 0 (len+1)
        else helper (fpos+1) len
      end
    end
  in
  helper 0 1

let pattern d =
  if is_terminating d then ""
  else pattern_search (mantissa d) d

let search denoms =
  let rec helper rem longest lpat lpat_len =
    match rem with
    | [] -> longest,lpat
    | hd::tl when hd < String.length lpat -> longest,lpat
    | hd::tl ->
        let hdpat = pattern hd in
        let hdpat_len = String.length hdpat in
        if hdpat_len > lpat_len then helper tl hd hdpat hdpat_len
        else helper tl longest lpat lpat_len
  in
  match denoms with
  | [] -> failwith "Empty"
  | hd::tl -> helper tl hd (pattern hd) (String.length (pattern hd))

let solution,sol_pat = search (List.rev (List.filter is_prime (seq 7 999)))
  
let main () =
  print_int solution;
  print_newline ();;

main();;
*)

(* Decent solution--
 * I own _The Book of Numbers_ by Conway and Guy, so why the hell didn't I use
 * it?  Because it's been too long since I read it.
 *)

open Primes

let is_long_prime p =
  let rec helper k residue = 
    let nres = 10*residue mod p in
    if nres = 1 then k = p - 1
    else helper (k+1) nres in
  if is_prime p then helper 1 1
  else false

let rec search ubd =
  if is_long_prime ubd then ubd
  else search (ubd - 1)

let solution = search 999

let main () = print_int solution;print_newline();;

main();;
