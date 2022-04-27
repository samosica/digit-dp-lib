let (<$>) f x = List.map f x
let (<*>) fs xs = List.concat_map (fun f -> List.map f xs) fs
let rec rep n l =
  if n = 0 then [[]] else List.cons <$> l <*> rep (n - 1) l
let enum n = List.init n Fun.id
let rec digits ~radix n =
  if n = 0 then [] else (n mod radix) :: digits ~radix (n / radix)
let int_of_digits ~radix l =
  List.fold_right (fun i acc -> i + acc * radix) l 0
