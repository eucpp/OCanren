open GT
open MiniKanren
open Std

let ifte : goal -> goal -> goal -> goal =
  fun c t e -> (c &&& t) ||| ((?~c) &&& e)

let rec findo p e xs =
  Fresh.three (fun x xs' ys' -> ?&
    [ xs === x % xs'
    ; ifte (p x)
        (e === x)
        (findo p e xs')
    ]
  )

let rec removeo p xs ys = ?|
  [ (xs === nil ()) &&& (ys === nil ())
  ; Fresh.three (fun x xs' ys' -> ?&
      [ xs === x % xs'
      ; ifte (p x)
          (ys === xs')
          (ys === x % ys' &&& removeo p xs' ys')
      ]
    )
  ]

let rec filtero p xs ys = ?|
  [ (xs === nil ()) &&& (ys === nil ())
  ; Fresh.three (fun x xs' ys' -> ?&
    [ (xs === x % xs')
    ; ifte (p x)
        (ys === x % ys')
        (ys === ys')
    ; filtero p xs' ys'
    ])
  ]

let test_cnt = ref 0

let test_l ~n g =
  let show ll =
    GT.show(List.logic) (GT.show(logic) string_of_int) ll
  in
  let print ll =
    Printf.printf "%s\n%!" @@ show ll
  in
  let i = !test_cnt in
  test_cnt := 1 + !test_cnt;
  Printf.printf "Test #%d:\n" i;
  List.iter print @@ Stream.take ~n @@
    g (fun t -> t#reify (List.reify reify));
  Printf.printf "-------------------\n"

let test_ll ~n g =
  let show ll =
    GT.show(List.logic) (GT.show(List.logic) (GT.show(logic) string_of_int)) ll
  in
  let print ll =
    Printf.printf "%s\n%!" @@ show ll
  in
  let i = !test_cnt in
  test_cnt := 1 + !test_cnt;
  Printf.printf "Test #%d:\n" i;
  List.iter print @@ Stream.take ~n @@
    g (fun t -> t#reify (List.reify (List.reify reify)));
  Printf.printf "-------------------\n"

let _ =
  let list = List.list in

  let p l = Fresh.one (fun x -> l === list [x]) in

  test_l ~n:5 @@
    run q (fun q ->
      Fresh.three (fun a b c ->
        findo p q (list [list [a; b]; list [!!1]; list [c]; nil ()])
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      Fresh.one (fun x ->
        findo p x q
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      Fresh.three (fun a b c ->
        removeo p (list [list [a; b]; list [!!1]; list [c]; nil ()])
                  (list [list [a; b]; list [c]; nil ()])
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      Fresh.three (fun a b c ->
        removeo p (list [list [a; b]; list [!!1]; list [c]; nil ()]) q
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      removeo p q q
    );

  test_ll ~n:5 @@
    run q (fun q ->
      removeo p q (list [nil ()])
    );

  test_ll ~n:5 @@
    run q (fun q ->
      Fresh.three (fun a b c ->
        filtero p (list [list [a; b]; list [c]; list [!!1]; nil ()])
                  (list [list [c]; list [!!1]])
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      Fresh.three (fun a b c ->
        filtero p (list [list [a; b]; list [c]; list [!!1]; nil ()]) q
      )
    );

  test_ll ~n:5 @@
    run q (fun q ->
      filtero p q (list [list [!!1]])
    );

  test_ll ~n:5 @@
    run q (fun q ->
      filtero p q (nil ())
    );

  test_ll ~n:5 @@
    run q (fun q ->
      filtero p q q
    );

  ()
