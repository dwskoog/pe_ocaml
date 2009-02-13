let multiples_less_than a l =
  let rec seq_helper a n l =
    if n >= l then []
    else n :: seq_helper a (a+n) l
  in
    seq_helper a a l

let bounded_fib_seq l =
  let rec helper seq =
    match seq with
      x::y::tail -> if x + y <= l then helper ((x+y)::seq) else seq
    | _ -> failwith "two_seeds_need"
  in
    helper [1;1]

