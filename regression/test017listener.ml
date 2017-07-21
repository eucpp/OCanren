open MiniKanren
(* open GT *)

let show_llist = GT.show(List.logic) (GT.show(logic) (string_of_int))

let p : (int List.ground, int logic List.logic) printer = fun rr -> show_llist @@ rr#refine (List.reify ManualReifiers.int) ~inj:(List.to_logic to_logic)

let rec appendo a b ab =
  Trace.(trace three) (fun q r s -> Listener.Goal ("appendo", [p q; p r; p s])) a b ab
  (conde
    [ ((unify ~p a @@ nil ()) &&& (unify ~p b ab))
    ; fresh (h t ab')
        (unify ~p a (h%t))
        (unify ~p (h%ab') ab)
        (appendo t b ab')
    ]
  (* &&&
    ((Trace.(trace three)) (fun q r s -> Listener.Answer ("appendo", [p q; p r; p s])) a b ab success) *)
  )

let rec reverso a b =
  Trace.(trace two) (fun q r -> Listener.Goal ("reverso", [p q; p r])) a b
  (conde
    [ ((unify ~p a @@ nil ()) &&& (unify ~p b @@ nil ()))
    ; fresh (h t a')
        (unify ~p a (h%t))
        (appendo a' !<h b)
        (defer (reverso t a'))
    ]
  (* &&&
    ((Trace.(trace two)) (fun q r -> Listener.Answer ("reverso", [p q; p r])) a b success) *)
  )

let f a b = (a === b) &&& (b === !!1)

let g a b = ?& [(a === b); (b === !!1); (a === !!2)]

let _ =
  let logger = TreeLogger.create () in
  let stream = run ~listener:(logger :> Listener.t) q (fun q -> reverso q q) (fun qs -> qs) in
  let _ = Stream.take ~n:3 stream in
  logger#print Format.std_formatter

(* let _ =
  let logger = TreeLogger.create () in
  let stream = run ~listener:(logger :> Listener.t) q (fun q -> f q q) (fun qs -> qs) in
  let _ = Stream.take ~n:1 stream in
  logger#print Format.std_formatter *)
