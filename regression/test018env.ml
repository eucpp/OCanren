open MiniKanren
open Tester

module Option =
  struct
    module T =
      struct
        type 'a t = 'a option
        let fmap f x = GT.gmap(GT.option) f x
      end

    include T
    include Fmap1(T)

    type 'a tg = 'a T.t
    type 'a tl = 'a T.t logic
    type ('a, 'b) ti = ('a tg, 'b tl) injected

    let some x  = inj @@ distrib (Some x)
    let none () = inj @@ distrib None

    let rec show_ground sa x = GT.show(GT.option) sa x
    let rec show_logic  sa x = GT.show(logic) (GT.show(GT.option) (GT.show(logic) sa)) x
  end

module Tree =
  struct
    module T =
      struct
        @type ('a, 't) t = Nil | Node of 'a * 't * 't with show, gmap

        let fmap f g x = GT.gmap(t) f g x
     end

    include T
    include Fmap2 (T)

    let nil ()      = inj @@ distrib Nil
    let node a l r  = inj @@ distrib @@ Node (a, l, r)

    type 'a tg = ('a, 'a tg) t
    type 'a tl = ('a , 'a tl) t logic
    type ('a, 'b) ti = ('a tg, 'b tl) injected

    let rec show_ground sa x = GT.show(t) sa (show_ground sa) x
    let rec show_logic  sa x = GT.show(logic) (GT.show(t) (GT.show(logic) sa) (show_logic sa)) x
  end

let inj_option env = Option.inj_logic (inj_logic) (Mapping.create env)

let inj_tree env =
  let rec inj_tree' m = Tree.inj_logic inj_logic (inj_tree') m in
  inj_tree' @@ Mapping.create env

let _ =
  run_exn_with_env (Option.show_ground string_of_int) 1 q qh (REPR(fun env q ->
      (q === Option.some !!42) &&&
      (q === (inj_option env (Value (Some (Var (0, []))))))
  ));

  run_exn_with_env (Tree.show_ground string_of_int) 1 q qh (REPR(fun env q ->
    fresh (x)
      (q === Tree.(node !!42 x (nil ()))) &&&
      (q === inj_tree env Tree.(Value (Node (Var (0, []), Var (1, []), Var (1, [])))))
  ))
