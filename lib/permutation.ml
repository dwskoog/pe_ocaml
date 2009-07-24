(*
 * http://en.literateprograms.org/Kth_permutation_(OCaml)
 *)
let kth_perm xs k =
  let rec rr n k =
    if n = 0 then []
    else k mod n :: rr (n-1) (k/n) in
  let dfr xs =
    let f x rs =
      x :: List.map (fun r -> r + (if x <= r then 1 else 0)) rs in
    List.fold_right f xs [] in
  List.map (List.nth xs) (dfr (rr (List.length xs) k))
