(* PROBLEM --
 * In the 20×20 grid below, four numbers along a diagonal line have been marked
 * in red.
 *
 * 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
 * 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
 * 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
 * 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
 * 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
 * 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
 * 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
 * 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
 * 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
 * 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
 * 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
 * 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
 * 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
 * 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
 * 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
 * 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
 * 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
 * 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
 * 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
 * 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
 *
 * The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
 *
 * What is the greatest product of four adjacent numbers in any direction (up,
 * down, left, right, or diagonally) in the 20×20 grid?
 *
 * SOLUTION --
 *)

open Extensions

let search_space = [|
  [|08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08 |];
  [|49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00 |];
  [|81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65 |];
  [|52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91 |];
  [|22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80 |];
  [|24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50 |];
  [|32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70 |];
  [|67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21 |];
  [|24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72 |];
  [|21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95 |];
  [|78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92 |];
  [|16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57 |];
  [|86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58 |];
  [|19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40 |];
  [|04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66 |];
  [|88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69 |];
  [|04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36 |];
  [|20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16 |];
  [|20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54 |];
  [|01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48 |];
|]

let horz_prod grid len (x,y) =
  let row = grid.(x) in
  let row_length = Array.length row in
  let rec helper y len =
    if len < 1 then None
    else if y+len >= row_length + 1 then None
    else 
      let recur = helper (succ y) (pred len) in
      match recur with
      | None    -> Some(row.(y))
      | Some(r) -> Some(row.(y) * r)
  in
    helper y len

let vert_prod grid len (x,y) =
  let col_length = Array.length grid in
  let rec helper x len =
    if len < 1 then None
    else if x+len >= col_length + 1 then None
    else 
      let recur = helper (succ x) (pred len) in
      match recur with
      | None    -> Some(grid.(x).(y))
      | Some(r) -> Some(grid.(x).(y) * r)
  in
    helper x len

let rec right_diag_prod grid len (x,y) =
  let col_length = Array.length grid in
  if x >= col_length then None 
  else let row = grid.(x) in
  let row_length = Array.length row in
  let is_valid n m = n+len < col_length + 1 && m+len < row_length + 1 in
  if len < 1 then None
  else if not (is_valid x y) then None
  else  
    let recur = right_diag_prod grid (pred len) ((succ x),(succ y)) in
    match recur with
    | None    -> Some(row.(y))
    | Some(r) -> Some(row.(y) * r)

let rec left_diag_prod grid len (x,y) =
  let col_length = Array.length grid in
  if x >= col_length then None
  else let row = grid.(x) in
  let is_valid x y = x+len < col_length + 1 && y-len+1 >= 0 in
  if len < 1 then None
  else if not (is_valid x y) then None
  else
    let recur = left_diag_prod grid (pred len) ((succ x),(pred y)) in
    match recur with
    | None    -> Some(row.(y))
    | Some(r) -> Some(row.(y) * r)

let enum_coords grid =
  let col_length = Array.length grid in
  let row_length = Array.length (grid.(0)) in
  let rec helper x y =
    (x,y)::
    if y < row_length - 1 then helper x (succ y)
    else if x < col_length - 1 then helper (succ x) 0
    else []
  in
    helper 0 0

let enum_prods grid len = 
  let coords = enum_coords grid in
  let horz_prods = List.map (horz_prod grid len) coords in
  let vert_prods = List.map (vert_prod grid len) coords in
  let right_diag_prods = List.map (right_diag_prod grid len) coords in
  let left_diag_prods = List.map (left_diag_prod grid len) coords in
  let prods = 
    List.combine4 horz_prods vert_prods right_diag_prods left_diag_prods in
  List.combine coords prods

let print_prod oc ((x,y),(h,v,rd, ld)) = 
  let print_opt oc opt c =
    match opt with
    | None -> Printf.fprintf oc "    None%s" c
    | Some(r) -> Printf.fprintf oc "%08d%s" r c in
  let po = print_opt oc in
  Printf.fprintf oc "(%02d;%02d)," x y;
  po h ",";
  po v ",";
  po rd ",";
  po ld "\n"

let dump_prods oc prods = 
  let _ = List.map (print_prod oc) prods in ()

let main () = 
  let oc = open_out "prods.csv" in
  let prods = (enum_prods search_space 4) in
  dump_prods oc prods;
  close_out oc;;

main();;
