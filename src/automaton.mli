(* 'i : 入力アルファベット *)
(* 'o : 出力アルファベット *)
(* 's : 状態 *)
type ('i, 'o, 's) t_ =
  { init_state : 's;
    trans : 'i -> 's -> 'o * 's;
    accepts : 's -> bool }

(* 状態の型 's を隠したバージョン．こちらをメインに使う *)
type ('i, 'o) t = Automaton : ('i, 'o, 's) t_ -> ('i, 'o) t
 
val intersect : ('i, 'o1) t -> ('i, 'o2) t -> ('i, 'o1 * 'o2) t
val union : ('i, 'o1) t -> ('i, 'o2) t -> ('i, 'o1 * 'o2) t
val complement : ('i, 'o) t -> ('i, 'o) t
 
(* 文字列を構成するすべての文字が述語を満たすか判定するオートマトン *)
val for_all : ('i -> bool) -> ('i, unit) t
(* 文字列中に述語を満たす文字が存在するか判定するオートマトン *)
val exists : ('i -> bool) -> ('i, unit) t

(* [TODO] 状態数最小のオートマトンは2つ存在するので名前を変える *)
val min_automaton : unit -> ('i, unit) t

(* [TODO] leading zero を無視するオートマトンを作る関数を作る *)
