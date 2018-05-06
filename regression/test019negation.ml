open Printf
open MiniKanren
open Std
open Tester

let (!) = (!!)
let (!!) = List.list

let show_int_list   = GT.(show List.ground @@ show int)
let show_intl_llist = GT.(show List.logic @@ show logic @@ show int)

let runInt n = runR MiniKanren.reify GT.(show int) GT.(show logic @@ show int) n
let runIList n = runR (List.reify MiniKanren.reify) show_int_list show_intl_llist n

let _ =
  (* test simple cases *)
  runInt    (-1) q qh (REPR (fun q -> ?~(q === !1)));
  runInt    (-1) q qh (REPR (fun q -> ?~(q =/= !1)));
  runInt    (-1) q qh (REPR (fun q -> (q === !1) &&& ?~(q === !2)));
  runInt    (-1) q qh (REPR (fun q -> (q =/= !1) &&& ?~(q =/= !2)));

  (* test contradiction *)
  runInt    (-1) q qh (REPR (fun q -> ?~((q === !1) ||| (q =/= !1))));

  (* test that conjunction and disjunction under negation behave properly *)
  runIList  (-1) q qh (REPR (fun q -> (fresh (x y)   ((x =/= !1) ||| (y =/= !2)) (q === !![x; y]))));
  runIList  (-1) q qh (REPR (fun q -> (fresh (x y) ?~((x === !1) &&& (y === !2)) (q === !![x; y]))));

  runIList  (-1) q qh (REPR (fun q -> (fresh (x y)   ((x =/= !1) &&& (y =/= !2)) (q === !![x; y]))));
  runIList  (-1) q qh (REPR (fun q -> (fresh (x y) ?~((x === !1) ||| (y === !2)) (q === !![x; y]))));

  (* test lists (implicit conjunction) *)
  runIList  (-1) q qh (REPR (fun q -> (fresh (x y) ((q === !![x; y]) &&& ?~(!![x; y] === !![!1; !2])))));
  runIList  (-1) q qh (REPR (fun q -> (fresh (x y) ((q === !![x; y]) &&& ?~(!![x; y] =/= !![!1; !2])))));

  runIList  (-1) q qh (REPR (fun q -> (fresh (x)   (q =/= !![!1; x; !2]))));
  runIList  (-1) q qh (REPR (fun q -> (fresh (x) ?~(q === !![!1; x; !2]))));

  (* this test must succeed and derive constraint `forall (x). q =/= [x]`  *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) (q === !![x]))));

  (* the following two tests must fail *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) (q === !![x])) &&& (fresh (y) (q === !![y]))));
  runIList  (-1) q qh (REPR (fun q -> (fresh (y) (q === !![y])) &&& ?~(fresh (x) (q === !![x]))));

  (* this tests must fail because there is [x] such that [x === 1] (or [x === q]) *)
  runInt    (-1) q  qh (REPR (fun q -> ?~(fresh (x) (x === !1))));
  runInt    (-1) q  qh (REPR (fun q -> (q === !1) &&& ?~(fresh (x) (x === q))));
  runInt    (-1) q  qh (REPR (fun q -> ?~(fresh (x) (x === q))));

  (* this tests must fail because there is [x] such that [x === 1 \/ x === 2] (or [x === q \/ x === r]) *)
  runInt    (-1) q  qh  (REPR (fun q   -> ?~(fresh (x) ((x === !1) ||| (x === !2)))));
  runInt    (-1) qr qrh (REPR (fun q r -> (q === !1) &&& (r === !2) &&& ?~(fresh (x) ((x === q) ||| (x === r)))));
  runInt    (-1) qr qrh (REPR (fun q r -> ?~(fresh (x) ((x === q) ||| (x === r)))));

  (* these goals also should fail because there is [x] such that [x =/= q] (or [x =/= q \/ x =/= r]) *)
  runInt    (-1) q  qh  (REPR (fun q    -> ?~(fresh (x) (x =/= q))));
  runInt    (-1) qr qrh (REPR (fun q r  -> ?~(fresh (x) ((x =/= q) ||| (x =/= r)))));

  (* this test must fail because there is [x] such that [q =/= [x]] *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) (q =/= !![x]))));

  (* this test must derive constraint [q =/= r] *)
  runInt    (-1) qr qrh (REPR (fun q r -> ?~(fresh (x) ((x === q) &&& (x === r)))));

  (* this test must fail because there is [x] such that [x =/= q /\ x =/= r] *)
  runInt    (-1) qr qrh (REPR (fun q r -> ?~(fresh (x) ((x =/= q) &&& (x =/= r)))));

  (* this test is equal to `forall (x). exist (y) (x === y)` and should succeed *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) ?~(fresh (y) (x === y)))));

  (* this test is equal to `forall (x). exist (y) (x =/= y)` and should succeed *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) ?~(fresh (y) (x =/= y)))));

  (* this test is equal to `forall (x). exist (y) (q === y)` and should succeed *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) ?~(fresh (y) (q === y)))));

  (* this test is equal to `forall (x). exist (y) (q =/= y)` and should succeed *)
  runIList  (-1) q qh (REPR (fun q -> ?~(fresh (x) ?~(fresh (y) (q =/= y)))))


let runNat n = runR (Nat.reify) (fun n -> string_of_int @@ Nat.to_int n) (GT.show(Nat.logic)) n

let peano n =
  let rec peano' n x = conde [
    (n === x);
    (delay (fun () -> peano' n (Nat.succ x)));
  ] in
  peano' n Nat.zero

let _ =
  (* test that negation of infinite goals is able to terminate if arguments are ground enough *)
  runNat    (-1) q qh (REPR (fun q -> (fresh (n) (n === nat 3) ?~(peano n))))
