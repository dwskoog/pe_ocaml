(* PROBLEM --
 * The fraction ^(49)/_(98) is a curious fraction, as an inexperienced
 * mathematician in attempting to simplify it may incorrectly believe that
 * ^(49)/_(98) = ^(4)/_(8), which is correct, is obtained by cancelling the 9s.
 *
 * We shall consider fractions like, ^(30)/_(50) = ^(3)/_(5), to be trivial
 * examples.
 *
 * There are exactly four non-trivial examples of this type of fraction, less
 * than one in value, and containing two digits in the numerator and
 * denominator.
 *
 * If the product of these four fractions is given in its lowest common terms,
 * find the value of the denominator.
 *
 * SOLUTION --
 *)

open Extensions
open Numbers

let two_digit_ints = List.map string_of_int (seq 10 99)

let is_trivial a b = (a.[1] = '0' && b.[1] = '0')

let reduce (a,b) =
  if a.[1] = b.[0] then (String.slice a 0 0, String.slice b 1 1)
  else if a.[0] = b.[1] then (String.slice a 1 1, String.slice b 0 0)
  else (a,b)

let is_unorthodox (a,b) =
  if a >= b then false
  else let ra,rb = reduce (a,b) in
  if ra = a then false
  else if rb = "0" then false
  else if is_trivial a b then false
  else let ratio = Ratio.create_ratio_string a b in
  let r_ratio = Ratio.create_ratio_string ra rb in
  Ratio.eq_ratio ratio r_ratio

let candidates = List.flatten (List.cross two_digit_ints two_digit_ints)

let unorthodox = List.filter is_unorthodox candidates

let reduced_unortho = List.map reduce unorthodox

let solution =
  let f (x,y) = Ratio.create_ratio_string x y in
  let denom r = Big_int.int_of_big_int (Ratio.denominator_ratio r) in
  let ratios = List.map f reduced_unortho in
  let unit_ratio = Ratio.create_ratio_int 1 1 in
  let mult = Ratio.mult_ratio in
  let prod = Ratio.normalize_ratio(List.fold_left mult unit_ratio ratios) in
  denom prod

let _ = print_int solution; print_newline();;
