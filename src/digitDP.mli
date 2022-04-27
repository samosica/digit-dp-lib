open Nat
 
type bin = z s s
 
type _ term
 
val var : string -> 'a term
(* 負の整数には対応していない *)
val int : int -> 'a term
(* 11 進表記以降には対応していない *)
val str : string -> 'a term
(* 加算，減算はオペランドを符号なし整数として扱う *)
(* デフォルトではオーバーフローが生じたときは例外を発生させる *)
(* run, count 等のオプション引数 permit_overflow を true にすると *)
(* オーバーフロー発生時には mod 2^len が取られる *)
val (+@) : 'a term -> 'a term -> 'a term
val (-@) : 'a term -> 'a term -> 'a term
(* bitwise and *)
val (&@) : bin term -> bin term -> bin term
(* bitwise or *)
val (|@) : bin term -> bin term -> bin term
(* bitwise xor *)
val (^@) : bin term -> bin term -> bin term
(* bitwise negation *)
val (~~) : bin term -> bin term
 
type 'a rel
 
val (=@) : 'a term -> 'a term -> 'a rel
val (<=@) : 'a term -> 'a term -> 'a rel
val (<@) : 'a term -> 'a term -> 'a rel
val (>=@) : 'a term -> 'a term -> 'a rel
val (>@) : 'a term -> 'a term -> 'a rel

(* rem_rel t1 m r = t1 を m で割ったときの余りは r *)
val rem_rel : 'a term -> int -> int -> 'a rel
(* div_rel t i = t は i で割り切れる *)
val div_rel : 'a term -> int -> 'a rel
 
type 'a assignment = (string * 'a) list

exception Overflow 

type 'a constraint_
 
(* ConstrINT... *)
val cint_of_aut : (int * int assignment, 'o) Automaton.t -> 'a constraint_
val cint_of_rel : 'a s rel -> 'a s constraint_
 
val intersect : 'a constraint_ -> 'a constraint_ -> 'a constraint_
val union : 'a constraint_ -> 'a constraint_ -> 'a constraint_
val complement : 'a constraint_ -> 'a constraint_
 
(* 減算でオーバーフローが生じないという前提の下で，
 * すべての項の計算でオーバーフローが生じないような桁数を返す
 *)
val required_length : radix:'a s nat -> var_len:int -> 'a s rel -> int

(* 制約を満たすすべての数列 w = w[1] ... w[len] について，
 *   mul w[1] (mul w[2] (... (mul w[len-1] (mul w[len] accept_val))))
 * を計算し，その和を求める
 *)
(* int * 'i の int はそれまでに読んだ文字数 *)
val run :
  (module Monoid.S with type t = 'm) ->
  mul:(int assignment -> 'm -> 'm) ->
  accept_val:'m ->
  ?permit_overflow:bool ->
  radix:'a nat ->
  len:int ->
  vars:string list ->
  'a constraint_ list ->
  'm

(* 制約を満たす数列を数える *)
val count :
  (module Monoid.S with type t = 'm) ->
  one:'m ->
  ?permit_overflow:bool ->
  radix:'a nat ->
  len:int ->
  vars:string list ->
  'a constraint_ list ->
  'm

(* 制約を満たす数列を列挙する *)
(* 返り値のリストは制約を満たす変数の割り当てからなる *)
(* 数列は初項から順に並べたリストの形で表現されている *)
val enum :
  ?permit_overflow:bool ->
  radix:'a nat ->
  len:int ->
  vars:string list ->
  'a constraint_ list ->
  int list assignment list

(* 制約を満たす数列を列挙する *)
(* 数列を返す代わりにそれを radix 進表記に持つ整数を返す *)
val enum_int :
  ?permit_overflow:bool ->
  radix:'a nat ->
  len:int ->
  vars:string list ->
  'a constraint_ list ->
  int assignment list
