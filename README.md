# digit-dp-lib
桁DP，オートマトンDPを書くのを楽にするOCamlのライブラリです．
オートマトンからDPを生成するのに加えて，式の形で表された制約からDPを生成することが可能です．
**注意：まだ高速化等を行なっていないので実践向きではありません．**

## 使用例
### [禁止された数字（AtCoder Beginner Contest 007 D）](https://atcoder.jp/contests/abc007/tasks/abc007_4)
`a <= x`，`x <= b`（`a, b`は与えられる正整数），「`x`は4, 9を含む」を満たす整数`x`の個数を数える問題です．
この問題を解くコードは次のように書けます．

```ocaml
open DigitDPLib
open Automaton
open DigitDP

(* 個数を数えるのに使う「数」の定義 *)
(* 定義を変えることで，例えば，個数の mod 10^9 + 7 が求められる *)
module Int = struct
  type t = int
  let zero = 0
  let (+) a b = a + b
end

let forbidden_number a b =
  let cs = 
    [ cint_of_rel (int a <=@ var "x"); (* a <= x であるという制約 *)
      cint_of_rel (var "x" <=@ int b); (* x <= b であるという制約 *)
      cint_of_aut (exists (fun (_, env) -> (* 「x は 4, 9 を含む」という制約 *)
        List.mem (List.assoc "x" env) [4; 9])) ] in
  count (module Int) ~one:1 ~permit_overflow:true ~radix:Nat.ten ~len:20 ~vars:["x"] cs
```

### [Unfair Nim (AtCoder Beginner Contest 172 F)](https://atcoder.jp/contests/abc172/tasks/abc172_f)
1番目の山から2番目の山に移す石の個数を`x`とおきます．ニムで後手必勝となる必要十分条件は各山にある石の個数のXORが0になることなので，求める`x`は次の条件を満たす最小の非負整数です．ただし，`^`はbitwise xorを表します．
- `x < A[1]`
- `(A[1] - x) ^ (A[2] + x) = A[3] ^ ... ^ A[N]`

よって，この問題を解くコードは次のように書けます．
```ocaml
let unfair_nim a = (* a には各山の石の個数が格納されている *)
  let a1 :: a2 :: rest = a in
  let r = List.fold_left (lxor) 0 rest in (* 3番目以降の山にある石の個数の XOR *)
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
```

### `min(x, y) = 480, x, y < 10^3`
`m = min(x, y)`であるための必要十分条件は「「`x >= y`かつ`m = y`」または「`y >= x`かつ`m = x`」」です．
このことを用いると，この問題を解くコードは次のように書けます．

```ocaml
let () =
  let cs =
    [ union
        (intersect
           (cint_of_rel (var "x" >=@ var "y"))
           (cint_of_rel (var "m" =@ var "y")))
        (intersect
           (cint_of_rel (var "y" >=@ var "x"))
           (cint_of_rel (var "m" =@ var "x")));
      cint_of_rel (var "m" =@ int 480) ] in
  let c = count (module Int) ~one:1 ~radix:Nat.ten ~len:3 ~vars:["x"; "y"; "m"] cs in
  print_int c
```
同様の方法で絶対値`|・|`も扱うことができます．

数列を上から見ている場合はこの手法は使わなくていいのですが，和を下から計算するときなど数列を下から見る場合に必要となります．

### 2525を含む長さ7の数字列
[polymorphic variant](https://ocaml.org/manual/polyvariant.html)を用いることで状態の型を明示的に書く必要がありません．（これがOCamlを使っている理由のひとつです）

```ocaml
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
  print_int c
```

## 今後の方針
- もっと書きやすくする（Haskellのライブラリの知見が使える気がするのですが理解できていません）
- もっと効率的にする（状態を整数で扱うようにする，同じ項に対するオートマトンを使いまわすなどして生成するオートマトンを小さくする，状態数最小化を行なうなど）
- 正しく動作することを形式検証する（比較的やりやすいはず）
