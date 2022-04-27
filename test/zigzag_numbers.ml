(* 問題元（2021年9月15日確認） *)
(*   JOI 2011-2012 予選 問題6 - ジグザグ数 *)
(*   (https://www.ioi-jp.org/joi/2011/2012-yo-prob_and_sol/2012-yo-t6/2012-yo-t6.html) *)

open DigitDPLib
open Automaton
open DigitDP

module Int = struct
  type t = int
  let zero = 0
  let (+) a b = a + b
end

let zigzag_numbers a b m =
  let zigzag_aut =
    Automaton 
      { init_state = `Q0;
        trans = (fun (_, env) s ->
          let x = List.assoc "x" env in
          let s' =
            match s with
              `Q0 -> `Q1 x
            | `Q1 y ->
               if y < x then
                 `QUp x
               else if y > x then
                 `QDown x
               else if x = 0 then
                 `QLZ
               else
                 `QFail
            | `QUp y ->
               if y > x then
                 `QDown x
               else if x = 0 then
                 `QLZ
               else
                 `QFail
            | `QDown y ->
               if y < x then
                 `QUp x
               else if x = 0 then
                 `QLZ
               else
                 `QFail
            | `QLZ -> if x = 0 then `QLZ else `QFail
            | `QFail -> `QFail in
          ((), s'));
        accepts = (fun s -> s <> `QFail) } in
  let cs = 
    [ cint_of_rel (int a <=@ var "x");
      cint_of_rel (var "x" <=@ int b);
      cint_of_rel (div_rel (var "x") m);
      cint_of_aut zigzag_aut ] in
  count (module Int) ~one:1 ~radix:Nat.ten ~len:64 ~vars:["x"] cs

let%expect_test "zigzag_numbers" =
  let a = 6 in
  let b = 1234567 in
  let m = 3 in
  let c = zigzag_numbers a b m in
  print_int c;
  [%expect {| 50246 |}]
