open DigitDPLib
open DigitDP

module Int = struct
  type t = int
  let zero = 0
  let (+) a b = a + b
end

(* [n; ...; m - 1] *)
let range n m = List.init (m - n) (fun i -> n + i)

let%expect_test "eq" =
  let cs =
    [ cint_of_rel (int 3 =@ int 3) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:4 ~vars:[] cs in
  print_int c;
  [%expect{| 1 |}]

let%expect_test "le" =
  let cs =
    [ cint_of_rel (int 19 <=@ var "x") ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:4 ~vars:["x"] cs in
  print_int c;
  [%expect{| 9981 |}] (* 10000 - 19 *)

let%test "le_enum" =
  let cs =
    [ cint_of_rel (int 19 <=@ var "x") ] in
  let sol =
    enum ~radix:Nat.ten ~len:4 ~vars:["x"] cs
    |> List.map (fun a -> ListExt.int_of_digits ~radix:10 (List.assoc "x" a))
    |> List.sort compare in
  sol = range 19 10000

let%expect_test "lt" =
  let cs =
    [ cint_of_rel (var "x" <@ int 81) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:4 ~vars:["x"] cs in
  print_int c;
  [%expect{| 81 |}] (* 81 - 0 *)

let%expect_test "le_lt" =
  let cs =
    [ cint_of_rel (int 19 <=@ var "x");
      cint_of_rel (var "x" <@ int 81) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:4 ~vars:["x"] cs in
  print_int c;
  [%expect{| 62 |}] (* = 81 - 19 *)

let%expect_test "and" =
  let cs =
    [ cint_of_rel (var "x" &@ var "y" =@ int 0b11010) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.two ~len:5 ~vars:["x"; "y"] cs in
  print_int c;
  (* x, y の上から1，2，4桁目は 1 で確定で， *)
  (* 残りの2桁は (0, 0), (0, 1), (1, 0) のいずれか *)
  [%expect{| 9 |}] (* = 3^2 *)

let%test "and_enum" =
  let cs =
    [ cint_of_rel (var "x" &@ var "y" =@ int 0b11010) ] in
  let sol =
    enum ~radix:Nat.two ~len:5 ~vars:["x"; "y"] cs
    |> List.map (fun a ->
           List.map (fun (id, l) -> (id, ListExt.int_of_digits ~radix:2 l)) a)
    |> List.sort compare in
  let expected =
    ListExt.rep 2 [(0, 0); (0, 1); (1, 0)]
    |> List.map (fun l ->
           let (a, b) =
             List.fold_left2 (fun (a, b) (i, j) k ->
                 (a + i * (1 lsl k), b + j * (1 lsl k))) (0b11010, 0b11010) l [0; 2] in
           [("x", a); ("y", b)])
    |> List.sort compare in
  sol = expected

let%expect_test "or" =
  let cs =
    [ cint_of_rel (var "x" |@ var "y" =@ int 0b11010) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.two ~len:5 ~vars:["x"; "y"] cs in
  print_int c;
  (* x, y の上から3，5桁目は 0 で確定で， *)
  (* 残りの3桁は (0, 1), (1, 0), (1, 1) のいずれか *)
  [%expect{| 27 |}] (* = 3^3 *)

let%expect_test "xor" =
  let cs =
    [ cint_of_rel (var "x" ^@ var "y" =@ int 0b11010) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.two ~len:5 ~vars:["x"; "y"] cs in
  print_int c;
  (* x, y の上から1，2，4桁目は等しく（それぞれ2通り）， *)
  (* 残りの2桁は異なる（同じく2通りずつ） *)
  [%expect{| 32 |}] (* = 2^5 *)

let%expect_test "not" =
  let cs =
    [ cint_of_rel (~~ (int 0b11100) =@ int 0b00011) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.two ~len:5 ~vars:[] cs in
  print_int c;
  [%expect{| 1 |}]
  
let%expect_test "rightmost_one" =
  let cs =
    [ cint_of_rel (var "x" &@ (~~ (var "x") +@ int 1) =@ int 0b100) ] in
  let c = count (module Int) ~one:1 ~permit_overflow:true ~radix:Nat.two ~len:10 ~vars:["x"] cs in
  print_int c;
  (* 制約は「1 が立つビットのうち最も下位にあるのは（下から数えて）3つ目」と同値 *)
  (* そのような条件を満たす数は2進表記で ...100 の形をしている *)
  [%expect{| 128 |}] (* = 2^7 *)

let%expect_test "add_overflow" =
  let cs =
    [ cint_of_rel (var "x" +@ int 1 <=@ int 9) ] in
  begin
    try
      let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:1 ~vars:["x"] cs in
      print_int c
    with
      Overflow -> print_string "Detect overflow";
  end;
  (* x = 9 のときにオーバーフローが生じる *)
  [%expect{| Detect overflow |}]

let%expect_test "add_permit_overflow" =
  let cs =
    [ cint_of_rel (var "x" +@ var "y" =@ int 320);
      cint_of_rel (var "x" <=@ var "y") ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~permit_overflow:true ~len:10 ~vars:["x"; "y"] cs in
  print_int c;
  (* 解は *)
  (* - (x, 320 - x) (0 <= x <= 160) *)
  (* - (x, 10^10 + 320 - x) (321 <= x <= 5 * 10^9 + 160) *)
  (* の2種類 *)
  (* permit_overflow を true にすると，オーバーフロー発生時 mod 2^len が取られることに注意 *)
  [%expect{| 5000000001 |}] (* = 5_000_000_001 *)

let%expect_test "add_permit_overflow_with_limit" =
  let cs =
    [ cint_of_rel (var "x" +@ var "y" =@ int 320);
      cint_of_rel (var "x" <=@ var "y");
      cint_of_rel (var "y" <=@ int 320) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~permit_overflow:true ~len:10 ~vars:["x"; "y"] cs in
  print_int c;
  (* 解は (x, 320 - x) (0 <= x <= 160) の形で表せる *)
  [%expect{| 161 |}]


let%expect_test "sub_overflow" =
  let cs =
    [ cint_of_rel (var "x" -@ int 13 =@ var "x" -@ int 13);
      cint_of_rel (var "x" <@ int 10) ] in
  begin
    try
      let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:1 ~vars:["x"] cs in
      print_int c
    with
      Overflow -> print_string "Detect overflow";
  end;
  [%expect{| Detect overflow |}]

let%expect_test "sub_permit_overflow" =
  let cs =
    [ cint_of_rel (var "x" -@ var "y" =@ int 1);
      cint_of_rel (var "x" >=@ var "y") ] in
  let c = count (module Int) ~one:1 ~permit_overflow:true ~radix:Nat.ten ~len:3 ~vars:["x"; "y"] cs in
  print_int c;
  [%expect{| 999 |}]

let%expect_test "abs" =
  let cs =
    [ union
        (intersect
           (cint_of_rel (var "x" >=@ var "y"))
           (cint_of_rel (var "x" -@ var "y" =@ int 2)))
        (intersect
           (cint_of_rel (var "y" >=@ var "x"))
           (cint_of_rel (var "y" -@ var "x" =@ int 2))) ] in
  let c = count (module Int) ~one:1 ~permit_overflow:true ~radix:Nat.ten ~len:3 ~vars:["x"; "y"] cs in
  print_int c;
  (* 制約は |x - y| = 2 と同値 *)
  (* 解の個数は次の2つの合計 *)
  (* - 0, ..., 998 (= 2 * 499) から連続する2つの数を選び，一方を x，もう一方を y とする（499 * 2 個） *)
  (* - 1, ..., 999 (= 1 + 2 * 499) から連続する2つの数を選び，一方を x，もう一方を y とする（499 * 2 個） *)
  [%expect{| 1996 |}]

let%expect_test "min" =
  let cs =
    [ union
        (intersect
           (cint_of_rel (var "x" >=@ var "y"))
           (cint_of_rel (var "min" =@ var "y")))
        (intersect
           (cint_of_rel (var "y" >=@ var "x"))
           (cint_of_rel (var "min" =@ var "x")));
      cint_of_rel (var "min" =@ int 480) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:3 ~vars:["x"; "y"; "min"] cs in
  print_int c;
  (* min x y = 480 となるためには片方が 480 に等しく，もう片方が 480 以上である必要がある *)
  (* 480 以上の3桁の数は 1000 - 480 = 520 個であり， *)
  (* 両方 480 のケースを重複して数えないよう注意すると， *)
  (*   520 * 2 - 1 = 1039 個 *)
  [%expect{| 1039 |}]

let%expect_test "digit sum" =
  let open Automaton in
  let digit_sum_aut =
    Automaton
      { init_state = 0;
        trans = (fun (_, a) s -> ((), List.assoc "x" a + s));
        accepts = (fun s -> s = 8) } in
  let cs =
    [ cint_of_rel (var "x" <=@ int 99);
      cint_of_aut digit_sum_aut ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:2 ~vars:["x"] cs in
  print_int c;
  [%expect{| 9 |}]
