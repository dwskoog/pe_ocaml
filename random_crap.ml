let break_list lst =
  let revlst = List.rev lst in
  let rec helper rem accum =
    let prev = List.hd accum in
    match rem with
      h::t when h < prev -> helper t (h::accum)
    | _ -> (List.rev rem), accum
  in
  if List.length lst > 0 then helper (List.tl revlst) [List.hd revlst]
  else [], []

