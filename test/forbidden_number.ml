(* 問題元（2021年9月15日確認） *)
(*   AtCoder Beginner Contest 007 D - 禁止された数字 *)
(*   (https://atcoder.jp/contests/abc007/tasks/abc007_4) *)

open DigitDPLib
open Automaton
open DigitDP

module Int = struct
  type t = int
  let zero = 0
  let (+) a b = a + b
end

let forbidden_number a b =
  let cs = 
    [ cint_of_rel (int a <=@ var "x");
      cint_of_rel (var "x" <=@ int b);
      cint_of_aut (for_all (fun (_, env) -> 
        not (List.mem (List.assoc "x" env) [4; 9]))) ] in
  let cnt = count (module Int) ~one:1 ~permit_overflow:true ~radix:Nat.ten ~len:20 ~vars:["x"] cs in  
  (b - a + 1) - cnt

(* 上記 URL の入力例 4 *)
let%expect_test "forbidden_number" =
  let a = 1 in
  let b = 1_000_000_000_000_000_000 in
  print_int (forbidden_number a b);
  [%expect{| 981985601490518016 |}]
