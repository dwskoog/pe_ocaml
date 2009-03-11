(* PROBLEM --
 * 
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
 * containing over five-thousand first names, begin by sorting it into
 * alphabetical order. Then working out the alphabetical value for each name,
 * multiply this value by its alphabetical position in the list to obtain a name
 * score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
 * would obtain a score of 938 × 53 = 49714.
 *
 * What is the total of all the name scores in the file?
 *
 * SOLUTION --
 *)

open Extensions
open String

let break name_str =
  let len = String.length name_str in
  let rec helper names offset =
    if offset > len then names
    else let name,noff = strtok name_str ',' offset in
    helper (name::names) noff
  in
  helper [] 0

let names =
  let str_name = 
    let chan = open_in "names.txt" in
    let str = input_line chan in
    close_in chan; str
  in
  List.sort String.compare (break str_name)

let char_value c = 1 + (Char.code (Char.uppercase c)) - (Char.code 'A')

let name_value name = 
  let rec helper total i =
    if i = String.length name then total
    else if name.[i] = '"' then helper total (i+1)
    else helper (total + (char_value name.[i])) (i+1)
  in
  helper 0 0

let name_values = List.map name_value names
let value_positions = Numbers.seq 1 (List.length name_values)

let accum_ns x y z = x + (y * z)

let value = List.fold_left2 accum_ns 0 name_values value_positions

let main() = print_int value; print_newline();;

main();;
