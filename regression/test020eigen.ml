open Printf
open MiniKanren
open Std
open Tester

let (!) = (!!)
let (!!) = List.list

let show_int_pair   = GT.(show Pair.ground (show int) (show int))
let show_intl_lpair = GT.(show Pair.logic (show logic @@ show int) (show logic @@ show int))

let show_int_list   = GT.(show List.ground @@ show int)
let show_intl_llist = GT.(show List.logic @@ show logic @@ show int)

let runInt n = runR MiniKanren.reify GT.(show int) GT.(show logic @@ show int) n
let runIPair n = runR (Pair.reify MiniKanren.reify MiniKanren.reify) show_int_pair show_intl_lpair n
let runIList n = runR (List.reify MiniKanren.reify) show_int_list show_intl_llist n

let (!!) = MiniKanren.(!!)
let list = MiniKanrenStd.List.list

let _ =

  (* forall x. q === x --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x -> q === x))
  );

  (* forall x. exists y. x === y --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        x === y
      )
    )
  ));

  (* forall x. exists y. q === x --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (x === y) &&& (q === y)
      )
    )
  ));

  (* forall x. exists y. q === x --- should fail (same as previous) *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (x === y) &&& (y === q)
      )
    )
  ));

  (* forall x. exists y. q === (x,x) --- should fail *)
  runIPair (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (y === pair x x) &&& (y === q)
      )
    )
  ));

  (* forall x. exists y. q === [x] --- should fail *)
  runIList (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (q === !< y) &&& (y === x)
      )
    )
  ));

  (* forall x. exists y. y === [1; 2; 3; x; 4] --- should succeed *)
  runIList (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (y === list [!!1; !!2; !!3; x; !!4])
      )
    )
  ));

  (* forall x. exists y. x === [1; 2; 3; y; 4] --- should fail *)
  runIList (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (x === list [!!1; !!2; !!3; y; !!4])
      )
    )
  ));

  (* exists x. forall y. x === [1; 2; 3; y; 4] --- should fail *)
  runIList (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.one (fun y ->
        (x === list [!!1; !!2; !!3; y; !!4])
      )
    )
  ));

  (* exists x. forall y. y === [1; 2; 3; x; 4] --- should fail *)
  runIList (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.one (fun y ->
        (y === list [!!1; !!2; !!3; x; !!4])
      )
    )
  ));

  (* forall hd tl. exists x y z. Cons hd tl === [x; y; z] --- should fail *)
  runIList (-1) q qh (REPR (fun q ->
    Eigen.two (fun hd tl ->
      Fresh.three (fun x y z ->
        (hd % tl === list [x; y; z])
      )
    )
  ));

  (* forall x. exists y. (x, q) === (y, y) --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        (pair q x === pair y y)
      )
    )
  ));

  (* forall x y. exists z. (x, y) === (z, z) --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.one (fun z ->
        (pair x y === pair z z)
      )
    )
  ));

  (* forall x. x =/= q --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      x =/= q
    )
  ));

  (* forall x. x =/= 5 --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      x =/= !!5
    )
  ));

  (* forall x y. x =/= y --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      x =/= y
    )
  ));

  (* forall x. exists y. x =/= y --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        x =/= y
      )
    )
  ));

  (* forall x. x =/= x --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
        x =/= x
    )
  ));

  (* forall x. x =/= 1 :: x --- should succeed (occurs-check) *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
        x =/= !!1 % x
    )
  ));

  (* forall x y. exists a b. (x, a) =/= (y, b) --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.two (fun a b ->
          pair x a =/= pair y b
      )
    )
  ));

  (* forall x y. q =/= [x; y] --- should succeed (and derive constraint) *)
  runIPair (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      q =/= pair x y
      (* we should use list here, because techically the query above
       * should fail (once we'll add type constraints),
       * but for some reason it doesn't derive constraint for list
       *)
      (* q =/= list [x; y] *)
    )
  ));

  (* forall x. x =/= (1; q) --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      (* possible bug here, with query `forall x y` *)
      x =/= pair !!1 q
    )
  ));

  (* forall x. exists y. (5, x) =/= (6, y) --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        pair !!5 x =/= pair !!6 y
      )
    )
  ));

  (* exists x. forall y. (5, x) =/= (6, y) --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.one (fun y ->
        pair !!5 x =/= pair !!6 y
      )
    )
  ));

  (* forall x. exists y. (q, x) =/= (y, y) --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        pair q x =/= pair y y
      )
    )
  ));

  (* forall x. x =/= [1; 2; 3; q; 4] --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
        x =/= list [!!1; !!2; !!3; q; !!4]
    )
  ));

  (* forall x. exists y. y =/= [1; 2; 3; x; 4] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        y =/= list [!!1; !!2; !!3; x; !!4]
      )
    )
  ));

  (* exists x. forall y. x =/= [1; 2; 3; y; 4] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.one (fun y ->
        x =/= list [!!1; !!2; !!3; y; !!4]
      )
    )
  ));

  (* forall x. x =/= [q] --- should fail *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      x =/= list [q]
    )
  ));

  (* forall x. exists y. y =/= [x] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.one (fun x ->
      Fresh.one (fun y ->
        y =/= list [x]
      )
    )
  ));

  (* forall x y. exists z. z =/= [x; y] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.one (fun z ->
        z =/= list [x; y]
      )
    )
  ));

  (* exists x. forall y z. x =/= [y; z] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.two (fun y z ->
        x =/= list [y; z]
      )
    )
  ));

  (* exists x. x === [1] /\ forall y z. x =/= [y; z] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      (x === list [!!1]) &&& Eigen.two (fun y z ->
        x =/= list [y; z]
      )
    )
  ));

  (* unsound *)
  (* exists x. forall y z. x === [1] /\ x =/= [y; z] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Fresh.one (fun x ->
      Eigen.two (fun y z ->
        (x === list [!!1]) &&& (x =/= list [y; z])
      )
    )
  ));

  (* forall x y. exists z. z =/= [x; y] /\ z === [1] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.one (fun z ->
        (z =/= list [x; y]) &&& (z === list [!!1])
      )
    )
  ));

  (* forall x y. exists z. z === [1] /\ z =/= [x; y] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.one (fun z ->
        (z === list [!!1]) &&& (z =/= list [x; y])
      )
    )
  ));

  (* forall x y. exists z. z =/= [x; y] /\ exists a b. z === [a; b] --- should succeed *)
  runInt (-1) q qh (REPR (fun q ->
    Eigen.two (fun x y ->
      Fresh.one (fun z ->
        (z =/= list [x; y]) &&& Fresh.two (fun a b ->
          z === list [a; b]
        )
      )
    )
  ));

  ()