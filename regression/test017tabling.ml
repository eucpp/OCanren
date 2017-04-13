open MiniKanren
open Tester

let arco x y = conde [
  (x === !!"a") &&& (y === !!"b");
  (x === !!"b") &&& (y === !!"a");
  (x === !!"b") &&& (y === !!"d");
]

let rec patho x y = conde [
  arco x y;
  Fresh.one (fun z ->
    (arco x z) &&& (patho z y)
  )
]

let tbl = make_table ()

let rec pathot x y = conde [
  arco x y;
  Fresh.one (fun z ->
    (arco x z) &&& (tabled tbl pathot z y)
  )
]

let show s = s

let _ =
  run_exn show 10 q qh (REPR(fun q -> patho !!"a" q));
  run_exn show 10 q qh (REPR(fun q -> tabled tbl pathot !!"a" q))
