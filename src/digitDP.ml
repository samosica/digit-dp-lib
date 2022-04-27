open Nat
open Automaton
open ListExt
 
type bin = z s s
 
type _ term =
  Var : string -> 'a term
| Int : int -> 'a term
| IntArray : int array -> 'a term (* 自然数の r 進表記を格納する *)
| Str : string -> 'a term
| Add : 'a term * 'a term -> 'a term
| Sub : 'a term * 'a term -> 'a term
| And : bin term * bin term -> bin term
| Or : bin term * bin term -> bin term
| Xor : bin term * bin term -> bin term
| Not : bin term -> bin term
 
let var id = Var id
let int i = Int i
let str s = Str s
let (+@) t1 t2 = Add (t1, t2)
let (-@) t1 t2 = Sub (t1, t2)
let (&@) t1 t2 = And (t1, t2)
let (|@) t1 t2 = Or (t1, t2)
let (^@) t1 t2 = Xor (t1, t2)
let (~~) t = Not t
 
type 'a rel =
  EqRel of 'a term * 'a term
| LeRel of 'a term * 'a term
| LtRel of 'a term * 'a term
| RemRel of 'a term * int * int
 
let (=@) t1 t2 = EqRel (t1, t2)
let (<=@) t1 t2 = LeRel (t1, t2)
let (<@) t1 t2 = LtRel (t1, t2)
let (>=@) t1 t2 = LeRel (t2, t1)
let (>@) t1 t2 = LtRel (t2, t1)

let rem_rel t m r = RemRel (t, m, r)
let div_rel t m = RemRel (t, m, 0)
 
type 'a assignment = (string * 'a) list

exception Overflow

type 'a constraint_ =
  Constraint : (bool -> (* permit_overflow *)
                'a nat -> (* radix *)
                (int * int assignment, 'o) Automaton.t) -> 
               'a constraint_
 
let cint_of_aut a = Constraint (fun _ _ -> a)
 
let rec aut_of_term : type a. permit_overflow:bool ->
                              radix:int ->
                              a term ->
                              (int * int assignment, int) Automaton.t = 
  fun ~permit_overflow ~radix t ->
  (* Var, IntArray, Str に対するオートマトンを生成する *)
  let aut_of_val out =
    Automaton
      { init_state = ();
        trans = (fun i _ -> (out i, ()));
        accepts = Fun.const true } in
  (* Add, Sub 以外の二項演算に対するオートマトンを生成する *)
  let aut_of_bop t1 t2 f =
    let Automaton a1 = aut_of_term ~permit_overflow ~radix t1 in
    let Automaton a2 = aut_of_term ~permit_overflow ~radix t2 in
    Automaton
      { init_state = (a1.init_state, a2.init_state);
        trans = (fun i (s1, s2) ->
          let (o1, s1') = a1.trans i s1 in
          let (o2, s2') = a2.trans i s2 in
          (f o1 o2, (s1', s2')));
        accepts = (fun (s1, s2) -> a1.accepts s1 && a2.accepts s2) } in
  match t with
    Var id ->
     aut_of_val (fun (_, assignment) -> List.assoc id assignment)
  | Int _ ->
     failwith "Not implemented: aut_of_term does not allow terms with Int nodes"
  | IntArray a ->
     aut_of_val (fun (i, _) ->
         let l = Array.length a in
         if i < l then a.(i) else 0)
  | Str s ->
     aut_of_val (fun (i, _) ->
         let l = String.length s in
         if i < l
         then Char.code s.[l - 1 - i] - Char.code '0'
         else 0)
  | Add (t1, t2) ->
     let Automaton a1 = aut_of_term ~permit_overflow ~radix t1 in
     let Automaton a2 = aut_of_term ~permit_overflow ~radix t2 in
     Automaton
       { init_state = (0 (* carry *), a1.init_state, a2.init_state);
         trans = (fun i (c, s1, s2) ->
           let (o1, s1') = a1.trans i s1 in
           let (o2, s2') = a2.trans i s2 in
           ((c + o1 + o2) mod radix, ((c + o1 + o2) / radix, s1', s2')));
         accepts = (fun (c, s1, s2) ->
           if not permit_overflow && c > 0 then
             raise Overflow
           else
             a1.accepts s1 && a2.accepts s2) }
  | Sub (t1, t2) ->
     let Automaton a1 = aut_of_term ~permit_overflow ~radix t1 in
     let Automaton a2 = aut_of_term ~permit_overflow ~radix t2 in
     Automaton
       { init_state = (0 (* borrow *), a1.init_state, a2.init_state);
         trans = (fun i (b, s1, s2) ->
           let (o1, s1') = a1.trans i s1 in
           let (o2, s2') = a2.trans i s2 in
           ((o1 + (radix - o2) + (radix - b)) mod radix,
            ((if o1 < o2 + b then 1 else 0), s1', s2')));
         accepts = (fun (b, s1, s2) ->
           if not permit_overflow && b > 0 then
             raise Overflow
           else
             a1.accepts s1 && a2.accepts s2) }
  | And (t1, t2) ->
     aut_of_bop t1 t2 ( land )
  | Or (t1, t2) ->
     aut_of_bop t1 t2 ( lor )
  | Xor (t1, t2) ->
     aut_of_bop t1 t2 ( lxor )
  | Not t ->
     let Automaton a = aut_of_term ~permit_overflow ~radix t in
     Automaton
       { init_state = a.init_state;
         trans = (fun i s ->
           let (o, s') = a.trans i s in
           (1 - o, s'));
         accepts = a.accepts }
 
let rec refine_term : type a. radix:int ->
                              a term ->
                              a term = 
  fun ~radix t ->
  match t with
    Int i -> IntArray (Array.of_list (digits ~radix i))
  | Var _ | IntArray _ | Str _ -> t
  | Add (t1, t2) -> Add (refine_term ~radix t1, refine_term ~radix t2)
  | Sub (t1, t2) -> Sub (refine_term ~radix t1, refine_term ~radix t2)
  | And (t1, t2) -> And (refine_term ~radix t1, refine_term ~radix t2)
  | Or (t1, t2) -> Or (refine_term ~radix t1, refine_term ~radix t2)
  | Xor (t1, t2) -> Xor (refine_term ~radix t1, refine_term ~radix t2)
  | Not t -> Not (refine_term ~radix t)
 
let aut_of_term : type a. permit_overflow:bool ->
                          radix:a s nat ->
                          a s term ->
                          (int * int assignment, int) Automaton.t =
  fun ~permit_overflow ~radix t ->
  let radix = int_of_nat radix in
  aut_of_term ~permit_overflow ~radix (refine_term ~radix t)
 
let aut_of_rel : type a. permit_overflow:bool ->
                         radix:a s nat ->
                         a s rel ->
                         (int * int assignment, unit) Automaton.t = 
  fun ~permit_overflow ~radix rel ->
  let aut_of_cmp t1 t2 p =
    let Automaton a1 = aut_of_term ~permit_overflow ~radix t1 in
    let Automaton a2 = aut_of_term ~permit_overflow ~radix t2 in
    let trans i (ord, s1, s2) =
      let (o1, s1') = a1.trans i s1 in
      let (o2, s2') = a2.trans i s2 in
      let ord' = Int.compare o1 o2 in
      let ord' =
        match ord, ord' with
          0, _ -> ord'
        | _, 0 -> ord
        | _, _ -> ord' in
      ((), (ord', s1', s2')) in
    Automaton
      { init_state = (0, a1.init_state, a2.init_state);
        trans = trans;
        accepts = (fun (ord, s1, s2) ->
          p ord && a1.accepts s1 && a2.accepts s2) } in
  match rel with
    RemRel (t, m, r) ->
     let Automaton a = aut_of_term ~permit_overflow ~radix t in
     let radix = int_of_nat radix in
     Automaton
       { init_state = (0, a.init_state);
         trans = (fun (i, assignment) (rem, s) ->
           let (o, s') = a.trans (i, assignment) s in
           let rem' = (rem + o * Math.mod_expt radix i m mod m) mod m in
           ((), (rem', s')));
         accepts = (fun (rem, s) ->
           (rem = r) && a.accepts s) }
  | EqRel (t1, t2) -> aut_of_cmp t1 t2 (fun ord -> ord = 0)
  | LeRel (t1, t2) -> aut_of_cmp t1 t2 (fun ord -> ord <= 0)
  | LtRel (t1, t2) -> aut_of_cmp t1 t2 (fun ord -> ord < 0)
 
let cint_of_rel rel =
  Constraint (fun permit_overflow radix -> aut_of_rel ~permit_overflow ~radix rel)

let intersect (Constraint g1) (Constraint g2) =
  Constraint (fun permit_overflow radix ->
      intersect (g1 permit_overflow radix) (g2 permit_overflow radix))
 
let union (Constraint g1) (Constraint g2) =
  Constraint (fun permit_overflow radix ->
      union (g1 permit_overflow radix) (g2 permit_overflow radix))
 
let complement (Constraint g) =
  Constraint (fun permit_overflow radix ->
      complement (g permit_overflow radix))
 
let rec required_length : type a. radix:a s nat -> 
                                  var_len:int ->
                                  a s term ->
                                  int =
  fun ~radix ~var_len t ->
  let f t1 t2 =
    max (required_length ~radix ~var_len t1) (required_length ~radix ~var_len t2) in
  match t with
    Var _ -> var_len
  | Int 0 -> 1
  | Int i -> List.length (digits ~radix:(int_of_nat radix) i)
  | IntArray [| |] -> 1
  | IntArray a -> Array.length a
  | Str s -> String.length s
  | Add (t1, t2) -> f t1 t2 + 1
  | Sub (t1, t2) -> f t1 t2
  | And (t1, t2) -> f t1 t2
  | Or (t1, t2) -> f t1 t2
  | Xor (t1, t2) -> f t1 t2
  | Not t -> required_length ~radix ~var_len t
 
let required_length ~radix ~var_len = function
    EqRel (t1, t2) | LeRel (t1, t2) | LtRel (t1, t2) ->
     max (required_length ~radix ~var_len t1) (required_length ~radix ~var_len t2)
  | RemRel (t, _, _) ->
     required_length ~radix ~var_len t

let run (type m)
  (module M : Monoid.S with type t = m)
  ~mul ~accept_val
  ?(permit_overflow = false)
  ~radix ~len ~vars cs =
  let alpha = enum (int_of_nat radix) in
  let alpha = List.map (fun vals -> List.combine vars vals) (rep (List.length vars) alpha) in
  let Constraint g =
    match cs with
      [] -> Constraint (fun _ _ -> true_automaton ())
    | c :: cs -> List.fold_left intersect c cs in
  AutomatonDP.run (module M) ~mul ~accept_val ~alpha ~len (g permit_overflow radix)
 
let count (type m)
  (module M : Monoid.S with type t = m)
  ~one ?(permit_overflow = false) ~radix ~len ~vars cs =
  run (module M) ~mul:(fun _ c -> c) ~accept_val:one ~permit_overflow ~radix ~len ~vars cs

let enum ?(permit_overflow = false) ~radix ~len ~vars cs =
  let module M = struct
      type t = int list assignment list
      let zero = []
      let (+) l1 l2 = l1 @ l2
    end in
  let mul a1 as2 =
    List.map (fun a2 -> 
        List.map2 (fun (id, hd) (_, tl) -> (id, hd :: tl)) a1 a2) as2 in
  let accept_val = [List.map (fun id -> (id, [])) vars] in
  run (module M) ~mul ~accept_val ~permit_overflow ~radix ~len ~vars cs

let enum_int ?(permit_overflow = false) ~radix ~len ~vars cs =
  let module M = struct
      type t = int assignment list
      let zero = []
      let (+) l1 l2 = l1 @ l2
    end in
  let r = int_of_nat radix in
  let mul a1 as2 =
    List.map (fun a2 -> 
        List.map2 (fun (id, x) (_, y) -> (id, x + y * r)) a1 a2) as2 in
  let accept_val = [List.map (fun id -> (id, 0)) vars] in
  run (module M) ~mul ~accept_val ~permit_overflow ~radix ~len ~vars cs
