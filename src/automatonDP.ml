open Automaton

(* 最初に Automaton.t_ に対する run を定義して，その後で Automaton.t に対する run を定義する
 * 直接定義しようとするとうまくいかない（存在型と let module の相性が悪い？）
 *)
let run (type m s)
  (module M : Monoid.S with type t = m)
  ~mul ~accept_val
  ~alpha ~len a =
  let module PairMap =
    Map.Make (struct
        type t = int * s
        let compare = compare
      end) in
  let tbl = ref PairMap.empty in
  let rec aux i s =
    if i = len then
      if a.accepts s then
        accept_val
      else
        M.zero
    else
      match PairMap.find_opt (i, s) !tbl with
        Some c -> c (* 計算済み *)
      | None ->
         let c = List.fold_left (fun acc j ->
           let (_, s') = a.trans (i, j) s in
           M.( + ) acc (mul j (aux (i + 1) s'))) M.zero alpha in
         tbl := PairMap.add (i, s) c !tbl;
         c in
  aux 0 a.init_state
 
let run (type m)
  (module M : Monoid.S with type t = m)
  ~mul ~accept_val
  ~alpha ~len
  (Automaton a) = run (module M) ~mul ~accept_val ~alpha ~len a
 
let count (type m)
  (module M : Monoid.S with type t = m)
  ~one ~alpha ~len a =
  run (module M) ~mul:(fun _ c -> c) ~accept_val:one ~alpha ~len a

let enum (type i) ~alpha ~len a =
  let module M = struct
      type t = i list list
      let zero = []
      let (+) l1 l2 = l1 @ l2
    end in
  let mul x ls = List.map (fun l -> x :: l) ls in
  let accept_val = [[]] in
  run (module M) ~mul ~accept_val ~alpha ~len a

