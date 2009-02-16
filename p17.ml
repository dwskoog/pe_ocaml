(* PROBLEM --
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 * in words, how many letters would be used?
 *
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
 * letters. The use of "and" when writing out numbers is in compliance with
 * British usage.
 *
 * SOLUTION --
 *)

open Extensions
open List
open Numbers

let names = [ 
  1,"one"; 2,"two"; 3,"three"; 4,"four"; 5,"five"; 6,"six"; 7,"seven";
  8,"eight"; 9,"nine"; 10,"ten"; 11,"eleven"; 12,"twelve"; 13,"thirteen";
  14,"fourteen"; 15,"fifteen"; 16,"sixteen"; 17,"seventeen"; 18,"eighteen";
  19,"nineteen"; 20,"twenty"; 30,"thirty"; 40,"forty"; 50,"fifty";
  60,"sixty"; 70,"seventy"; 80,"eighty"; 90,"ninety"
]

let rec name_of_num n =
  if n > 1000 then failwith "Unsupported beyond 1000"
  else if n < 1 then failwith "Positive integer only"
  else if n = 1000 then "one thousand"
  else if n >= 100 then
    let huns = n / 100 in
    let tens = n mod 100 in
    let tens_name = if tens = 0 then "" else " and " ^ (name_of_num tens) in
    (name_of_num huns) ^ " hundred" ^ tens_name
  else if mem_assoc n names then assoc n names
  else 
    let tens = (n / 10) * 10 in
    let ones = n mod 10 in
    (name_of_num tens) ^ "-" ^ (name_of_num ones)

let is_character c = c <> ' ' &&  c <> '-'

let main () =
  let nums = seq 1 1000 in
  let num_names = map name_of_num nums in
  let letters = flatten (map (String.explode) num_names) in
  let keepers = filter is_character letters in
  print_int (length keepers);
  print_newline();;

main();;

