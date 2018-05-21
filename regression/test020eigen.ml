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
  runInt    (-1) q qh (REPR (fun q -> Eigen.one (fun x -> q === x)));
  runInt    (-1) q qh (REPR (fun q -> Eigen.one (fun x -> Fresh.one (fun y -> x === y))));
  runInt    (-1) q qh (REPR (fun q -> Eigen.one (fun x -> Fresh.one (fun y -> (x === y) &&& (q === y)))));
  runInt    (-1) q qh (REPR (fun q -> Eigen.one (fun x -> Fresh.one (fun y -> (x === y) &&& (y === q)))));
  ()
