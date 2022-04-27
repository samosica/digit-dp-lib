open DigitDPLib
open Automaton
open AutomatonDP

module Int = struct
  type t = int
  let zero = 0
  let (+) a b = a + b
end

module Mod1_000_000_007 = struct
  type t = int
  let zero = 0
  let modulo = 1_000_000_007
  let (+) a b = (a + b) mod modulo
end

let%expect_test "2525" =
  (* 2525 を含む文字列を受理する *)
  let niconico_aut =
    Automaton
      { init_state = `Qs;
        trans = (fun (_, i) s ->
          let s' =
            match s, i with
                 `Qs, 5 -> `Q5
            |    `Q5, 2 -> `Q25
            |   `Q25, 5 -> `Q525
            |  `Q525, 2 -> `Q2525
            | `Q2525, _ -> `Q2525
            |      _, 5 -> `Q5
            |      _, _ -> `Qs in
          ((), s'));
        accepts = (fun s -> s = `Q2525) } in
  let c = count (module Int) ~one:1 ~alpha:(ListExt.enum 10) ~len:7 niconico_aut in
  print_int c;
  (* ...2525... を数えて，...252525... の分を引く *)
  [%expect{| 3980 |}] (* = 4 * 10 ^ 3 - 2 * 10 *)

let fib n =
  let rec fib_aux n a b =
    if n = 0 then a else fib_aux (n - 1) b ((a + b) mod Mod1_000_000_007.modulo) in
  fib_aux n 1 2

let%test "fib" =
  (* 01 列のうち 1 が連続しないものを受理する *)
  let no_consecutive_1s_aut =
    Automaton
      { init_state = `Qs;
        trans = (fun (_, i) s ->
          let s' =
            match s, i with
               `Qs, _ -> `Q i
            |  `Qf, _ -> `Qf
            | `Q 1, 1 -> `Qf
            |    _, _ -> `Q i in
          ((), s'));
        accepts = (fun s -> s <> `Qf) } in
  let c = count (module Mod1_000_000_007) ~one:1 ~alpha:(ListExt.enum 2) ~len:400 no_consecutive_1s_aut in
  (* 1 が連続しない長さ n の 01 列の個数はフィボナッチ数と同じ漸化式を満たす．ただし，最初の2項は 1, 2 *)
  c = fib 400

let fib_seq n =
  let rec fib_seq_aux n a b =
    if n = 0 then a else
      let a' = List.map (fun l -> 1 :: 0 :: l) a in
      let b' = List.map (fun l -> 0 :: l) b in
      fib_seq_aux (n - 1) b (a' @ b') in
  fib_seq_aux n [[]] [[0]; [1]]

let%test "fib_enum" =
  (* 01 列のうち 1 が連続しないものを受理する *)
  let no_consecutive_1s_aut =
    Automaton
      { init_state = `Qs;
        trans = (fun (_, i) s ->
          let s' =
            match s, i with
               `Qs, _ -> `Q i
            |  `Qf, _ -> `Qf
            | `Q 1, 1 -> `Qf
            |    _, _ -> `Q i in
          ((), s'));
        accepts = (fun s -> s <> `Qf) } in
  let sol = enum ~alpha:(ListExt.enum 2) ~len:10 no_consecutive_1s_aut in
  List.sort compare sol = List.sort compare (fib_seq 10)
