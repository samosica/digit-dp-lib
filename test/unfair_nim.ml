(* 問題元（2021年9月15日確認） *)
(*   AtCoder Beginner Contest 172 F - Unfair Nim *)
(*   (https://atcoder.jp/contests/abc172/tasks/abc172_f) *)

open DigitDPLib
open DigitDP

let unfair_nim a =
  let a1 :: a2 :: rest = a in
  let r = List.fold_left (lxor) 0 rest in
  let cs = 
    [ cint_of_rel (var "x" <@ int a1);
      cint_of_rel ((int a1 -@ var "x") ^@ (int a2 +@ var "x") =@ int r) ] in
  let module M = struct
      type t = int option
      let zero = None
      let (+) o1 o2 =
        match o1, o2 with
          None, _ -> o2
        | _, None -> o1
        | Some n, Some m -> Some (min n m)
    end in
  let mul env o = Option.map (fun j -> List.assoc "x" env + j * 2) o in
  let accept_val = Some 0 in
  run (module M) ~mul ~accept_val ~permit_overflow:true ~radix:Nat.two ~len:64 ~vars:["x"] cs

(* 上記 URL の入力例 5 *)
let%expect_test "unfair_nim" = 
  let a = [ 4294967297; 8589934593; 12884901890 ] in
  begin
    match unfair_nim a with
      None -> print_int (-1)
    | Some mn -> print_int mn
  end;
  [%expect {| 1 |}]

