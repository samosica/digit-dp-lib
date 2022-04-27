(*
 * オートマトンが受理する長さ len のすべての文字列 w = w[1] ... w[len] に対して，
 *   mul w[1] (mul w[2] (... (mul w[len-1] (mul w[len] accept_val))))
 * を計算し，その和を求める
 * 
 * モノイド 'm の二項演算 + と mul : 'i -> 'm -> 'm は分配則
 *   mul a (x + y) = mul a x + mul a y
 * を満たさなければならない
 * 
 * 参考（2021年9月14日確認）
 * - spaghetti-source (https://github.com/spaghetti-source/algorithm/blob/4fdac8202e26def25c1baf9127aaaed6a2c9f7c7/dynamic_programming/digit_dp.cc)
 * - オートマトン上の DP (桁 DP の一般化) (https://kuretchi.github.io/blog/entries/automaton-dp/)
 *)
(* int * 'i の int はそれまでに読んだ文字数 *)
val run :
  (module Monoid.S with type t = 'm) ->
  mul:('i -> 'm -> 'm) ->
  accept_val:'m ->
  alpha:'i list ->
  len:int ->
  (int * 'i, 'o) Automaton.t ->
  'm
 
(* オートマトンが受理する文字列を数える *)
val count :
  (module Monoid.S with type t = 'm) ->
  one:'m ->
  alpha:'i list ->
  len:int ->
  (int * 'i, 'o) Automaton.t ->
  'm    

(* オートマトンが受理する文字列を列挙する *)
(* 文字列の先頭は最初に読んだ文字 *)
val enum :
  alpha:'i list ->
  len:int ->
  (int * 'i, 'o) Automaton.t ->
  'i list list
