let max_option a b =
  match (a,b) with
  | None   ,None    -> None
  | Some(x),None    -> a
  | None   ,Some(y) -> b
  | Some(x),Some(y) -> Some(max x y)

module List =
  struct
    include List
    let combine2 = List.combine

    let rec combine3 alst blst clst =
      match(alst, blst, clst) with
      | ([],[],[]) -> []
      | (a::atl, b::btl, c::ctl) -> (a,b,c)::(combine3 atl btl ctl)
      | _ -> failwith "Even lists"

    let rec combine4 alst blst clst dlst =
      match(alst, blst, clst, dlst) with
      | ([],[],[], []) -> []
      | (a::atl, b::btl, c::ctl, d::dtl) -> 
          (a,b,c,d)::(combine4 atl btl ctl dtl)
      | _ -> failwith "Even lists"

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
