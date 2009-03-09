(* PROBLEM --
 * By starting at the top of the triangle below and moving to adjacent numbers
 * on the row below, the maximum total from top to bottom is 23.
 *
 * 3
 * 7 5
 * 2 4 6
 * 8 5 9 3
 *
 * That is, 3 + 7 + 4 + 9 = 23.
 *
 * Find the maximum total from top to bottom in triangle.txt (right click and
 * 'Save Link/Target As...'), a 15K text file containing a triangle with
 * one-hundred rows.
 *
 * NOTE: This is a much more difficult version of Problem 18. It is not possible
 * to try every route to solve this problem, as there are 2^(99) altogether! If
 * you could check one trillion (10^(12)) routes every second it would take over
 * twenty billion years to check them all. There is an efficient algorithm to
 * solve it. ;o)
 *
 * SOLUTION --
 *)
open Str

let size pyramid =
  let rec helper pyr sz =
  match pyr with
  | [] -> sz
  | hd::tl -> helper tl (sz+(List.length hd))
  in helper pyramid 0;;

let path_value pyramid =
  let memo = Hashtbl.create (size pyramid) in
  let rows = List.length pyramid in
  let rec helper i j =
  if i >= rows then 0
  else if Hashtbl.mem memo (i,j) then Hashtbl.find memo (i,j)
  else let lv = helper (i+1) j in
  let rv = helper (i+1) (j+1) in
  let v = (List.nth (List.nth pyramid i) j)+(max lv rv) in
  (Hashtbl.add memo (i,j) v;v)
  in helper 0 0;;

let row_of_string str = List.map int_of_string (split (regexp " ") str)

let read_all ic =
  let rec helper lines =
    try let line = input_line ic in
    helper (line::lines)
    with End_of_file -> lines
  in helper []

let pyramid =
  let ic = open_in "triangle.txt" in
  let lines = read_all ic in
  (close_in ic; List.rev_map row_of_string lines)

let solution = path_value pyramid

let _ = print_int solution;print_newline();;

