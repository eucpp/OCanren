open GT
open MiniKanren
open Std

module Combinators =
  struct

    @type 't term = I | K | S | App of 't * 't with show, gmap

    module X = Fmap(struct
      type 't t = 't term
      let fmap f = gmap(term) f
    end)

    let rec reify t = X.reify reify t

    let rec show t = GT.show(term) show  t
    let rec lshow t = GT.show(logic) (GT.show(term) lshow) t

    let print t =
      Printf.printf "%s\n%!" (show t)

    let lprint t =
      Printf.printf "%s\n%!" (lshow t)

    let i () = inj @@ X.distrib I
    let k () = inj @@ X.distrib K
    let s () = inj @@ X.distrib S

    let app t u = inj @@ X.distrib @@ App (t, u)

    (* Contraction *)
    let contr t u = conde
      [ (t === app (i ()) u)
      ; Fresh.one (fun p ->
          (t === app (app (k ()) u) p)
        )
      ; Fresh.three (fun x y z ->
          (t === app (app (app (s ()) x) y) z) &&&
          (u === app (app x z) (app y z))
        )
      ]

    (* One-step reduction *)
    let step t u = conde
      [ contr t u;
        Fresh.three (fun m n m' ->
          (t === app m  n) &&&
          (u === app m' n) &&&
          (contr m m')
        );
        Fresh.three (fun m n n' ->
          (t === app m n ) &&&
          (u === app m n') &&&
          (contr n n')
        )
      ]

    (* reduction *)
    let red = Tabling.(tabledrec two) (fun red t u -> conde
      [ (t === u)
      ; Fresh.one (fun t' ->
          (step t t') &&& (red t' u)
        )
      ]
    )

  end

let test ~n g =
  List.iter Combinators.lprint @@ Stream.take ~n @@
    g (fun t -> t#reify Combinators.reify)

let _ =
  let open Combinators in

  test ~n:10 @@
    run q (fun q ->
      Fresh.one (fun y ->
        red (app (app (k ()) (i ())) y) q
      )
    );

  ()
