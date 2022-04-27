type z = |
type _ s = |

type _ nat =
  Z : z nat
| S : 'n nat -> 'n s nat

let rec int_of_nat : type n. n nat -> int = function
    Z -> 0
  | S n -> 1 + int_of_nat n

let two = S (S Z)
let ten = S (S (S (S (S (S (S (S (S (S Z)))))))))
