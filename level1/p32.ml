(* PROBLEM --
 * We shall say that an n-digit number is pandigital if it makes use of all the
 * digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
 * through 5 pandigital.
 *
 * The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing
 * multiplicand, multiplier, and product is 1 through 9 pandigital.
 *
 * Find the sum of all products whose multiplicand/multiplier/product
 * identity can be written as a 1 through 9 pandigital.
 *
 * HINT: Some products can be obtained in more than one way so be sure to onlyi
 * include it once in your sum.
 *
 * SOLUTION --
 * Looking for all ab=c where a<b, drive the search based on a.
 * We only have to search through 98 since the product of 2 3+ digit numbers
 * can't satisfy the criteria.
 *)

open Extensions
open Numbers

let one_digit_as = seq 2 9

let two_digit_as = 
  let one_digit = seq 1 9 in
  let prod = List.flatten (List.cross one_digit one_digit) in
  let no_doubles = List.filter (fun (x,y) -> x <> y) prod in
  List.map (fun (x,y) -> 10*x+y) no_doubles

let multiplicands = one_digit_as @ two_digit_as

let list_of_int n =
  let rec helper n lst =
    if n < 10 then n::lst
    else helper (n/10) ((n mod 10)::lst)
  in helper n []

let is_zlpdp a b =
  let c = a * b in
  let digits = seq 1 9 in
  let a_ds = list_of_int a in
  let b_ds = list_of_int b in
  let c_ds = list_of_int c in
  let used_digits = List.sort compare (a_ds@b_ds@c_ds) in
  used_digits = digits

let splice n lst =
  let rec helper lst pos =
    if pos = 0 then n::lst
    else (List.hd lst)::(helper (List.tl lst) (pos-1))
  in List.map (helper lst) (seq 0 (List.length lst))

let rec list_choose lst n =
  match lst,n with
  | [],_ -> []
  | _,0  -> []
  | _,1  -> List.map (fun x -> [x]) lst
  |hd::tl,n ->
      let no_hds = list_choose tl n in
      let subs = list_choose tl (n-1) in
      no_hds @ (List.flatten (List.map (splice hd) subs))

let int_of_list lst =
  let rec helper sum lst =
    match lst with
    | [] -> sum
    | hd::tl -> helper (10*sum+hd) tl
  in helper 0 lst

let n_comp lst n = List.map int_of_list (list_choose lst n)

let complement a =
  let rem_digs = List.drop (seq 1 9) (list_of_int a) in
  let num_rem = List.length rem_digs in
  let comps = List.map (n_comp rem_digs) (seq 1 num_rem) in
  List.filter (is_zlpdp a) (List.flatten comps)

let pdps =
  let pdp x = (x, complement x) in
  let non_trivial (_,y) = y<>[] in
  List.filter non_trivial (List.map pdp multiplicands)

let prod (x,lst) = List.map (fun y -> x*y) lst

let prods = List.flatten (List.map prod pdps)

module Int_t = 
  struct
    type t = int
    let compare = Pervasives.compare
  end

module Int_set = Set.Make(Int_t)

let unique_prods =
  List.fold_left (fun x y -> Int_set.add y x) Int_set.empty prods

let solution = Int_set.fold ( + ) unique_prods 0

let _ = print_int solution; print_newline ();;

