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
  )

let filter = Listener.(function
  | Goal (_, _) -> true
  | Unif _  -> true
  | Diseq _ -> true
  | Disj    -> true
  | Conj    -> true
  | _ -> false
)

let _ =
  let logger = TreeLogger.create () in
  let stream = run ~listener:(logger :> Listener.t) q (fun q -> reverso q q) (fun qs -> qs) in
  let _ = Stream.take ~n:4 stream in
  logger#print ~filter Format.std_formatter
