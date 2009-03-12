let max_option a b =
  match (a,b) with
  | None   ,None    -> None
  | Some(x),None    -> a
  | None   ,Some(y) -> b
  | Some(x),Some(y) -> Some(max x y)

module List =
  struct
    include List

    let cross alst blst =
      let ecross a = rev(fold_left (fun x y -> (a,y)::x) [] blst) in
      let rec helper arem prod =
        match arem with
        | [] -> prod
        | hd::tl -> helper tl ((ecross hd )::prod)
      in
      rev(helper alst [])

    let ( |*| ) a b = cross a b

    let drop a b =
      let f x = not (List.mem x b) in
      List.filter f a

    let ( /- ) a b = drop a b

    let list_of_int n =
      let rec helper n lst =
        if n < 10 then n::lst
        else helper (n/10) ((n mod 10)::lst)
      in if n >= 0 then helper n []
      else (-1)::(helper (-n) [])

    let int_of_list lst =
      let rec helper sum lst =
        match lst with
        | [] -> sum
        | hd::tl when hd >= 0 -> helper (10*sum+hd) tl
        | _ -> failwith "Invalid representation"
      in match lst with
      | (-1)::tl -> - (helper 0 tl)
      | _ -> helper 0 lst

    let print_list p_el sep lst =
      let rec helper lst =
        match lst with
        | [] -> ()
        | hd::[] -> p_el hd;
        | hd::tl -> p_el hd; sep (); helper tl
      in print_char '['; helper lst; print_char ']'

    let print_int_list = print_list print_int (fun () -> print_char ';')
  end

module String =
  struct
    include String

    let explode str =
      let len = String.length(str) in
      let rec helper i l =
        if i >= 0 then helper (i-1) ((str.[i])::l)
        else l
      in
      helper (len-1) []

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

    let slice str first last =
      let len = last - first + 1 in
      String.sub str first len

    let strtok str sep offset =
      let has_sep = String.contains_from str offset sep in
      let index = if not has_sep then String.length str
                  else String.index_from str offset sep in
      (slice str offset (index-1)),(index+1)
  end
