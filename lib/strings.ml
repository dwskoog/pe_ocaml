let explode str =
  let rec len = String.length(str)
  and helper i l =
    if i < len then helper (i+1) ((str.[i])::l)
    else l
  in
    List.rev(helper 0 [])

let int_of_digit c =
  match c with
  | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3
  | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7
  | '8' -> 8 | '9' -> 9
  |  _  -> failwith (Printf.sprintf "Not a digit: %c" c)

let rec is_palindrome s =
  let len = String.length s in
  if len <= 1 then true
  else if s.[0] = s.[len - 1] then is_palindrome (String.sub s 1 (len-2))
  else false 

