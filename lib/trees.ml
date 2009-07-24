type 'a nary_tree = Tree of 'a * 'a nary_tree list

let rec transform_nary f (Tree(a, children)) =
  Tree(f(a), List.map (transform_nary f) children)

let rec preorder fmt (Tree(a, children)) =
  "(" ^
  (List.fold_left ( ^ ) (fmt a) (List.map (preorder fmt) children)) ^
  ")"

let nary_leaf a = Tree(a,[])

let nary_add_children (Tree(a, children)) new_children =
  Tree(a, children @ new_children)
