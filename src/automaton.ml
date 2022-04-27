type ('i, 'o, 's) t_ =
  { init_state : 's;
    trans : 'i -> 's -> 'o * 's;
    accepts : 's -> bool }
 
type ('i, 'o) t =
  Automaton : ('i, 'o, 's) t_ -> ('i, 'o) t
 
let intersect (Automaton a1) (Automaton a2) =
  Automaton
    { init_state = (a1.init_state, a2.init_state);
      trans = (fun i (s1, s2) -> 
        let (o1, s1') = a1.trans i s1 in
        let (o2, s2') = a2.trans i s2 in
        ((o1, o2), (s1', s2')));
      accepts = (fun (s1, s2) -> a1.accepts s1 && a2.accepts s2) }
 
let union (Automaton a1) (Automaton a2) =
  Automaton
    { init_state = (a1.init_state, a2.init_state);
      trans = (fun i (s1, s2) -> 
        let (o1, s1') = a1.trans i s1 in
        let (o2, s2') = a2.trans i s2 in
        ((o1, o2), (s1', s2')));
      accepts = (fun (s1, s2) -> a1.accepts s1 || a2.accepts s2) }
 
let complement (Automaton a) =
  Automaton { a with accepts = fun s -> not (a.accepts s) }

let true_automaton () =
  Automaton { init_state = ();
              trans = (fun _ _ -> ((), ()));
              accepts = Fun.const true }

let false_automaton () =
  Automaton { init_state = ();
              trans = (fun _ _ -> ((), ()));
              accepts = Fun.const false }
 
let for_all p =
  Automaton
    { init_state = true;
      trans = (fun i b -> ((), b && p i));
      accepts = Fun.id }
        
let exists p =
  Automaton
    { init_state = false;
      trans = (fun i b -> ((), b || p i));
      accepts = Fun.id }
