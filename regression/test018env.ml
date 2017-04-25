open MiniKanren
open Tester
(* open GT *)

(* module Tree =
  struct

    module T =
      struct
        @type ('a, 't) t = Nil | Node of 'a * 't * 't with show, gmap

        let fmap f g x = gmap(t) f g x
     end

  include T
  include Fmap2 (T)

  let nil = inj @@ distrib Nil
  let node a l r  = inj @@ distrib (Node (a, l, r))

  type tg = (int, tg) t
  type tl = (int logic, tl) t logic
  type ti = (tg, tl) injected

  let rec show_ground x = show(t) (string_of_int) (show_ground) x
  let rec show_logic  x = show(logic) (show(t) (show(logic) (string_of_int)) (show_logic)) x

end *)

(* let tree : Tree.tl = Tree.(Value (Node (Var (0, []), Var (1, []), Var (1, [])))) *)

(* let show = Tree.show_ground (string_of_int) *)

module Option =
  struct

    module T =
      struct
        type 'a t = 'a option
        let fmap f x = GT.gmap(GT.option) f x
      end

    include T
    include Fmap1(T)

    type tg = int T.t
    type tl = int logic T.t logic
    type ti = (tg, tl) injected

    let some x  = inj @@ distrib (Some x)
    let none () = inj @@ distrib None
  end

let inj_of_int : Mapping.t -> int logic -> (int, int logic) injected = fun _m -> function
   | Value n -> inj @@ lift n
   | Var (_,_) -> Obj.magic 1

let inj_option env =
  let m = Mapping.create env in
  (* let fuck1 : (Mapping.t -> 'b -> ('a, 'b) injected) =  *)
  let rec inj':(Mapping.t -> 'b -> ('a, 'b) injected) -> (Option.tl -> Option.ti) = fun f opt ->
    match opt with
    | Value opt -> Option.fmap (f m) opt |> Option.distrib |> inj
    | Var (_,_) -> Obj.magic 2

  in
  inj' inj_of_int
    (* inj_logic m @@ gmap(logic) Option.() opt *)

external llist_distrib : (('a,'c) injected, ('b,'d) injected) llist -> (('a, 'b) llist, ('c, 'd) llist) injected = "%identity"

let inj_list env :  int logic List.logic -> (int, int logic) List.groundi  =
  let m = Mapping.create env in
  (* let fuck1 : (Mapping.t -> 'b -> ('a, 'b) injected) =  *)
  let rec inj':(Mapping.t -> 'b -> ('a, 'b) injected) -> (Mapping.t -> 'd -> ('c, 'd) injected) -> Mapping.t -> int logic List.logic -> (int, int logic) List.groundi =
  fun f g m opt ->
    match opt with
    | Value opt -> GT.gmap(llist) (f m) (g m) opt |> llist_distrib |> inj
    | Var (_,_) -> Obj.magic 2
  in
  let rec shit x = inj' inj_of_int shit x in
  shit m

(* let rec inj_tree env tree =
  let m = Mapping.create env in
  let rec inj':(Tree.tl -> Tree.ti) = fun tree ->
    inj_logic m @@ gmap(logic) Tree.(fun t -> inj @@ distrib @@ fmap (inj_logic m) (inj') t) tree
  in
  42 *)
  (* let rec h = fun tree -> gmap(logic) Tree.(fun t -> inj @@ distrib @@ fmap (inj_logic m) (h) t) tree in *)
  (* let rec inj' = inj_logic m @@ h tree in *)
  (* let (_: int ) = inj' in *)
  (* let rec inj' = fun tree -> inj_logic m @@ gmap(logic) Tree.(fun t -> distrib @@ fmap (inj_logic m) (inj') t) tree in *)
  (* let tmp:int = Tree.(gmap(logic) (fun t -> distrib @@ fmap (inj_logic m) (inj_tree env) t) tree) in *)
  (* inj' *)
  (* inj_logic m @@ Tree.(gmap(logic) (fun t -> distrib @@ fmap (inj_logic m) (inj_logic m) t) tree) *)

(* let _ =
  run_exn_with_env show_ground 1 q qh (REPR(fun env q ->
    let t = inj_tree env tree in
    fresh (x)
      (q === Tree.(node !!42 x nil)) &&&
      (q === t)
  )) *)
