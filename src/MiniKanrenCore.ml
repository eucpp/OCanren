(*
 * MiniKanrenCode: miniKanren implementation.
 * Copyright (C) 2015-2017
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Printf

module Log =
  struct

    type t = < name    : string;
               count   : int;
               elapsed : float;
               enter   : unit;
               leave   : unit;
               subs    : t list;
               attach  : t -> unit;
               clear   : unit
             >

    class tc (name : string) =
      object
        val mutable count   = 0
        val mutable elapsed = float_of_int 0
        val mutable origin  = float_of_int 0
        val mutable depth   = 0
        val mutable subs    = ([] : t list)
        method name     = name
        method elapsed  = elapsed
        method count    = count
        method subs     = subs
        method attach l = subs <- l :: subs
        method clear    =
          count   <- 0;
          elapsed <- float_of_int 0;
          depth   <- 0;
          List.iter (fun l -> l#clear) subs
        method enter    =
          count <- count + 1;
          depth <- depth + 1;
          origin <- Unix.((times ()).tms_utime)
        method leave   =
          if depth > 0
          then elapsed <- elapsed +. Unix.((times ()).tms_utime) -. origin
          else failwith (sprintf "OCanren fatal (Log.leave): zero depth");
          depth <- depth - 1
      end

    let create parent name =
      let this = new tc name in
      match parent with
      | Some p -> p#attach this; this
      | None   -> this

    let run   = create None "run"
    let unify = create (Some run) "unify"

    let report () =
      let buf      = Buffer.create 1024      in
      let append s = Buffer.add_string buf s in
      let rec show o l =
        append @@ sprintf "%s%s: count=%d, time=%f\n" o l#name l#count l#elapsed;
        List.iter (show (o ^ "  ")) l#subs
      in
      show "" run;
      Buffer.contents buf

    let clear () = run#clear

  end

module Stream =
  struct
    type 'a t =
      | Nil
      | Cons of 'a * ('a t)
      | Thunk  of 'a thunk
      | Waiting of 'a suspended list
    and 'a thunk =
      unit -> 'a t
    and 'a suspended =
      {is_ready: unit -> bool; zz: 'a thunk}

    let nil         = Nil
    let single x    = Cons (x, Nil)
    let cons x s    = Cons (x, s)
    let from_fun zz = Thunk zz

    let suspend ~is_ready f = Waiting [{is_ready; zz=f}]

    let rec of_list = function
    | []    -> Nil
    | x::xs -> Cons (x, of_list xs)

    let force = function
    | Thunk zz  -> zz ()
    | xs        -> xs

    let rec mplus xs ys =
      match xs with
      | Nil           -> force ys
      | Cons (x, xs)  -> cons x (from_fun @@ fun () -> mplus (force ys) xs)
      | Thunk   _     -> from_fun (fun () -> mplus (force ys) xs)
      | Waiting ss    ->
        let ys = force ys in
        (* handling waiting streams is tricky *)
        match unwrap_suspended ss, ys with
        (* if [xs] has no ready streams and [ys] is also a waiting stream then we merge them  *)
        | Waiting ss, Waiting ss' -> Waiting (ss @ ss')
        (* if [xs] has no ready streams but [ys] is not a waiting stream then we swap them,
           pushing waiting stream to the back of the new stream *)
        | Waiting ss, _           -> mplus ys @@ from_fun (fun () -> xs)
        (* if [xs] has ready streams then [xs'] contains some lazy stream that is ready to produce new answers *)
        | xs', _ -> mplus xs' ys

    and unwrap_suspended ss =
      let rec find_ready prefix = function
        | ({is_ready; zz} as s)::ss ->
          if is_ready ()
          then Some (from_fun zz), (List.rev prefix) @ ss
          else find_ready (s::prefix) ss
        | [] -> None, List.rev prefix
      in
      match find_ready [] ss with
        | Some s, [] -> s
        | Some s, ss -> mplus (force s) @@ Waiting ss
        | None , ss  -> Waiting ss

    let rec bind s f =
      match s with
      | Nil           -> Nil
      | Cons (x, s)   -> mplus (f x) (from_fun (fun () -> bind (force s) f))
      | Thunk zz      -> from_fun (fun () -> bind (zz ()) f)
      | Waiting ss    ->
        match unwrap_suspended ss with
        | Waiting ss ->
          let helper {zz} as s = {s with zz = fun () -> bind (zz ()) f} in
          Waiting (List.map helper ss)
        | s          -> bind s f

    let rec msplit = function
    | Nil           -> None
    | Cons (x, xs)  -> Some (x, xs)
    | Thunk zz      -> msplit @@ zz ()
    | Waiting ss    ->
      match unwrap_suspended ss with
      | Waiting _ -> None
      | xs        -> msplit xs

    let is_empty s =
      match msplit s with
      | Some _  -> false
      | None    -> true

    let rec map f = function
    | Nil          -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Thunk zzz    -> from_fun (fun () -> map f @@ zzz ())
    | Waiting ss   ->
      let helper {zz} as s = {s with zz = fun () -> map f (zz ())} in
      Waiting (List.map helper ss)

    let rec iter f s =
      match msplit s with
      | Some (x, s) -> f x; iter f s
      | None        -> ()

    let rec filter p s =
      match msplit s with
      | Some (x, s) -> let s = filter p s in if p x then Cons (x, s) else s
      | None        -> Nil

    let rec fold f acc s =
      match msplit s with
      | Some (x, s) -> fold f (f acc x) s
      | None        -> acc

    let rec zip xs ys =
      match msplit xs, msplit ys with
      | None,         None          -> Nil
      | Some (x, xs), Some (y, ys)  -> Cons ((x, y), zip xs ys)
      | _                           -> invalid_arg "OCanren fatal (Stream.zip): streams have different lengths"

    let hd s =
      match msplit s with
      | Some (x, _) -> x
      | None        -> invalid_arg "OCanren fatal (Stream.hd): empty stream"

    let tl s =
      match msplit s with
      | Some (_, xs) -> xs
      | None         -> Nil

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match msplit s with
      | None          -> [], Nil
      | Some (x, s)  -> let xs, s = retrieve ~n:(n-1) s in x::xs, s

    let take ?n s = fst @@ retrieve ?n s

  end
;;

@type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a with show, gmap, html, eq, compare, foldl, foldr

let logic = {logic with
  gcata = ();
  plugins =
    object
      method gmap      = logic.plugins#gmap
      method html      = logic.plugins#html
      method eq        = logic.plugins#eq
      method compare   = logic.plugins#compare
      method foldl     = logic.plugins#foldl
      method foldr     = logic.plugins#foldr
      method show fa x =
        GT.transform(logic)
          (GT.lift fa)
          (object inherit ['a] @logic[show]
             method c_Var _ s i cs =
               let c = match cs with
               | [] -> ""
               | _  -> sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
               in
               sprintf "_.%d%s" i c
             method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
    end
}

module Var =
  struct
    type env    = int
    type scope  = int
    type anchor = int list

    let tabling_env = -1

    let unused_index = -1

    let non_local_scope = -6

    let tabling_scope = -7

    let new_scope =
      let scope = ref 0 in
      fun () -> (incr scope; !scope)

    let global_anchor = [-8]

    type t = {
      anchor        : anchor;
      env           : env;
      index         : int;
      mutable subst : Obj.t option;
      scope         : scope;
      constraints   : Obj.t list
    }

    let make ~env ~scope index = {
      env         = env;
      anchor      = global_anchor;
      subst       = None;
      constraints = [];
      index;
      scope;
    }

    let dummy =
      let env   = 0 in
      let scope = 0 in
      make ~env ~scope 0

    let valid_anchor anchor =
      anchor == global_anchor

    let reify r {index; constraints} =
      (index, List.map (fun x -> r @@ Obj.obj x) constraints)

    let equal x y =
      (x.index = y.index) && (x.env = y.env)

    let compare x y =
      if x.index <> y.index then x.index - y.index else x.env - y.env

    let hash x = Hashtbl.hash (x.env, x.index)
  end

module VarSet = Set.Make(Var)
module VarTbl = Hashtbl.Make(Var)

module VarMap =
  struct
    include Map.Make(Var)

    let update k f m =
      match f (try Some (find k m) with Not_found -> None) with
      | Some x -> add k x m
      | None   -> remove k m
  end

(* [Term] - encapsulates unsafe operations on OCaml's values extended with logic variables;
 *   provides set of functions to traverse these values
 *)
module Term :
  sig
    type t

    type tag = int

    val repr : 'a -> t

    (* [var x] if [x] is logic variable returns it, otherwise returns [None] *)
    val var : 'a -> Var.t option

    (* [is_valid_tag t] checks that tag is correct;
     *   (some OCaml's tags such as [closure_tag] are forbidden because
     *    the unification of such values is not supported currently)
     *)
    val is_valid_tag : tag -> bool

    (* [map ~fvar ~fval x] map over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val map : fvar:(Var.t -> t) -> fval:(tag -> t -> t) -> t -> t

    (* [iter ~fvar ~fval x] iteration over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val iter : fvar:(Var.t -> unit) -> fval:(tag -> t -> unit) -> t -> unit

    (* [fold ~fvar ~fval ~init x] fold over OCaml's value extended with logic variables;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar]
     *)
    val fold : fvar:('a -> Var.t -> 'a) -> fval:('a -> tag -> t -> 'a) -> init:'a -> t -> 'a

    exception Different_shape

    (* [fold ~fvar ~fval ~fvarval ~init x y] folds two OCaml's value extended with logic variables simultaneously;
     *   handles primitive types with the help of [fval] and logic variables with the help of [fvar];
     *   if it finds logic variable in one term but regular value in another in same place,
     *   it calls [fvarval];
     *   if two terms cannot be traversed simultaneously raises exception [Different_shape]
     *)
    val fold2 :
      fvar:('a -> Var.t -> Var.t -> 'a) ->
      fval:('a -> tag -> t -> t -> 'a)  ->
      fvarval:('a -> Var.t -> tag -> t -> 'a) ->
      init:'a -> t -> t -> 'a

    val equal   : t -> t -> bool
    val compare : t -> t -> int
    val hash    : t -> int
  end = struct
    type t = Obj.t
    type tag = int

    let repr = Obj.repr

    let var_tag, var_size =
      let dummy = Obj.repr Var.dummy in
      Obj.tag dummy, Obj.size dummy

    let is_var tx sx x =
      if (tx = var_tag) && (sx = var_size) then
         let anchor = (Obj.obj x : Var.t).Var.anchor in
         (Obj.is_block @@ Obj.repr anchor) && (Var.valid_anchor anchor)
      else false

    let is_box t =
      if (t <= Obj.last_non_constant_constructor_tag) &&
         (t >= Obj.first_non_constant_constructor_tag)
      then true
      else false

    let is_int = (=) Obj.int_tag
    let is_str = (=) Obj.string_tag
    let is_dbl = (=) Obj.double_tag

    let is_valid_tag t = (is_box t) || (is_int t) || (is_str t) || (is_dbl t)

    let var x =
      let x = Obj.repr x in
      let tx = Obj.tag x in
      if is_box tx then
        let sx = Obj.size x in
        if is_var tx sx x then Some (Obj.magic x) else None
      else None

    let rec map ~fvar ~fval x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar @@ Obj.magic x
        else
          let y = Obj.dup x in
          for i = 0 to sx - 1 do
            Obj.set_field y i @@ map ~fvar ~fval (Obj.field x i)
          done;
          y
      else
        fval tx x

    let rec iter ~fvar ~fval x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar @@ Obj.magic x
        else
          for i = 0 to sx - 1 do
            iter ~fvar ~fval (Obj.field x i)
          done;
      else
        fval tx x

    let rec fold ~fvar ~fval ~init x =
      let tx = Obj.tag x in
      if (is_box tx) then
        let sx = Obj.size x in
        if is_var tx sx x then
          fvar init @@ Obj.magic x
        else
          let rec inner i acc =
            if i < sx then
              let acc = fold ~fvar ~fval ~init:acc (Obj.field x i) in
              inner (i+1) acc
            else acc
          in
          inner 0 init
      else
        fval init tx x

    exception Different_shape

    let rec fold2 ~fvar ~fval ~fvarval ~init x y =
      let tx, ty = Obj.tag x, Obj.tag y in
      match is_box tx, is_box ty with
      | true, true -> begin
        let sx, sy = Obj.size x, Obj.size y in
        match is_var tx sx x, is_var ty sy y with
        | true, true    -> fvar init (Obj.magic x) (Obj.magic y)
        | true, false   -> fvarval init (Obj.magic x) ty y
        | false, true   -> fvarval init (Obj.magic y) tx x
        | false, false  ->
          if (tx = ty) && (sx = sy) then
            let fx, fy = Obj.field x, Obj.field y in
            let rec inner i acc =
              if i < sx then
                let acc = fold2 ~fvar ~fval ~fvarval ~init:acc (fx i) (fy i) in
                inner (i+1) acc
              else acc
            in
            inner 0 init
          else raise Different_shape
        end
      | true, false ->
        let sx = Obj.size x in
        if is_var tx sx x then fvarval init (Obj.magic x) ty y else raise Different_shape
      | false, true ->
        let sy = Obj.size y in
        if is_var ty sy y then fvarval init (Obj.magic y) tx x else raise Different_shape
      | false, false ->
        if tx = ty then fval init tx x y else raise Different_shape

    let equal = fold2 ~init:true
      ~fvar:(fun acc v u -> acc && (Var.equal v u))
      ~fval:(fun acc _ x y -> acc && (x = y))
      ~fvarval:(fun _ _ _ _ -> false)

    let compare = fold2 ~init:0
      ~fvar:(fun acc v u -> if acc <> 0 then acc else (Var.compare v u))
      ~fval:(fun acc _ x y -> if acc <> 0 then acc else (compare x y))
      ~fvarval:(fun _ _ _ _ -> -1)

    let hash = fold ~init:1
      ~fvar:(fun acc v -> Hashtbl.hash (acc, Var.hash v))
      ~fval:(fun acc _ x -> Hashtbl.hash (acc, Hashtbl.hash x))
  end

let (!!!) = Obj.magic

module Env :
  sig
    type t

    val empty         : unit -> t
    val create        : anchor:Var.env -> t
    val fresh         : scope:Var.scope -> t -> 'a
    val check         : t -> Var.t -> bool
    val check_exn     : t -> Var.t -> unit
    val is_var        : t -> 'a -> bool
    val var           : t -> 'a -> Var.t option
    val free_vars     : t -> 'a -> VarSet.t
    val has_free_vars : t -> 'a -> bool
    val merge         : t -> t -> t
  end =
  struct
    type t = {anchor : Var.env; mutable next : int}

    let last_anchor = ref 11
    let first_var = 10

    let empty () =
      incr last_anchor;
      {anchor = !last_anchor; next = first_var}

    let create ~anchor = {anchor; next = first_var}

    let fresh ~scope e =
      let v = !!!(Var.make ~env:e.anchor ~scope e.next) in
      e.next <- 1 + e.next;
      !!!v

    let check env v = (v.Var.env = env.anchor)

    let check_exn env v =
      if check env v then () else failwith "OCanren fatal (Env.check): wrong environment"

    let var env x =
      match Term.var x with
      | (Some v) as res -> check_exn env v; res
      | None            -> None

    let is_var env x = (var env x) <> None

    let free_vars env x =
      Term.fold (Term.repr x) ~init:VarSet.empty
        ~fvar:(fun acc v   -> VarSet.add v acc)
        ~fval:(fun acc t x ->
          if Term.is_valid_tag t then acc
          else failwith (sprintf "OCanren fatal (Env.free_vars): invalid value (%d)" t)
        )

    let has_free_vars env x =
      not (VarSet.is_empty @@ free_vars env x)

    let merge {anchor=anchor1; next=next1} {anchor=anchor2; next=next2} =
      assert (anchor1 == anchor2);
      {anchor=anchor1; next = max next1 next2}
  end

module Binding :
  sig
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    val is_relevant : Env.t -> VarSet.t -> t -> bool

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end =
  struct
    type t =
      { var   : Var.t
      ; term  : Term.t
      }

    let is_relevant env vs {var; term} =
      (VarSet.mem var vs) ||
      (match Env.var env term with Some v -> VarSet.mem v vs | None -> false)

    let equal {var=v; term=t} {var=u; term=p} =
      (Var.equal v u) || (Term.equal t p)

    let compare {var=v; term=t} {var=u; term=p} =
      let res = Var.compare v u in
      if res <> 0 then res else Term.compare t p

    let hash {var; term} = Hashtbl.hash (Var.hash var, Term.hash term)
  end

module Subst :
  sig
    type t

    val empty : t

    val of_list : Binding.t list -> t

    val split : t -> Binding.t list

    (* [project env subst x] - performs a deepwalk of term [x],
     *   replacing every variable to relevant binding in [subst];
     *   i.e. it obtains a value of term [x] in [subst] *)
    val project : Env.t -> t -> 'a -> 'a

    (* [refresh ?mapping ~scope dst_env src_env subst x] - takes a term [x],
     *   projects [subst] into it
     *   and replaces all stayed free variables
     *   into fresh variables in destination environment [dst_env].
     *   Returns a modified term along with a mapping from variables in [src_env] into [dst_env].
     *   Takes an optional argumet [mapping], in case it is passed a variable from [src_env]
     *   first is looked up in [mapping] and only if it is not present there
     *   fresh variable from [dst_env] is allocated.
     *)
    val refresh : ?mapping : Var.t VarTbl.t -> scope:Var.scope -> Env.t -> Env.t -> t -> 'a -> Var.t VarTbl.t * 'a

    (* [is_bound x subst] - checks whether [x] is bound by [subst] *)
    val is_bound : Var.t -> t -> bool

    (* [free_vars env subst x] - returns all free-variables of term [x] *)
    val free_vars : Env.t -> t -> 'a -> VarSet.t

    (* [unify env x y scope subst] returns None if two terms are not unifiable.
     *   Otherwise it returns a pair of prefix and new substituion.
     *   Prefix is a list of pairs (var, term) that were added to the original substituion.
     *)
    val unify : scope:Var.scope -> Env.t -> t -> 'a -> 'a -> (Binding.t list * t) option

    val reify : f:(Var.t -> Var.t) -> Env.t -> t -> 'a -> 'a

    (* [merge env s1 s2] merges two substituions *)
    val merge : Env.t -> t -> t -> t option

    (* [is_subsumed env s1 s2] checks that s1 is subsumed by s2 (i.e. s2 is more general than s1) *)
    val is_subsumed : Env.t -> t -> 'a -> 'a -> bool
  end =
  struct
    type t = Term.t VarMap.t

    let empty = VarMap.empty

    let of_list =
      ListLabels.fold_left ~init:empty ~f:(let open Binding in fun subst {var; term} ->
        VarMap.add var term subst
      )

    let split s = VarMap.fold (fun var term xs -> Binding.({var; term})::xs) s []

    type lterm = Var of Var.t | Value of Term.t

    let rec walk env subst x =
      (* walk var *)
      let rec walkv env subst v =
        Env.check_exn env v;
        match v.Var.subst with
        | Some term -> walkt env subst !!!term
        | None ->
            try walkt env subst (VarMap.find v subst)
            with Not_found -> Var v
      (* walk term *)
      and walkt env subst t =
        match Env.var env t with
        | Some v -> walkv env subst v
        | None   -> Value t
      in
      walkv env subst x

    (* same as [Term.map] but performs [walk] on the road *)
    let map ~fvar ~fval env subst x =
      let rec deepfvar v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar v
        | Value x -> Term.map x ~fval ~fvar:deepfvar
      in
      Term.map x ~fval ~fvar:deepfvar

    (* same as [Term.iter] but performs [walk] on the road *)
    let iter ~fvar ~fval env subst x =
      let rec deepfvar v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar v
        | Value x -> Term.iter x ~fval ~fvar:deepfvar
      in
      Term.iter x ~fval ~fvar:deepfvar

    (* same as [Term.fold] but performs [walk] on the road *)
    let fold ~fvar ~fval ~init env subst x =
      let rec deepfvar acc v =
        Env.check_exn env v;
        match walk env subst v with
        | Var v   -> fvar acc v
        | Value x -> Term.fold x ~fval ~fvar:deepfvar ~init:acc
      in
      Term.fold x ~init ~fval ~fvar:deepfvar

    exception Occurs_check

    let rec occurs env subst var term =
      iter env subst term
        (* ~fvar:(fun acc v -> acc || Var.equal v var) *)
        ~fvar:(fun v -> if Var.equal v var then raise Occurs_check)
        ~fval:(fun t x ->
          if Term.is_valid_tag t then ()
          else
            failwith (sprintf "OCanren fatal (Subst.occurs): invalid value (%d)" t)
        )

    let extend ~scope env subst var term  =
      (* if occurs env subst var term then raise Occurs_check *)
      occurs env subst var term;
        (* assert (Env.var env var <> Env.var env term); *)

      (* It is safe to modify variables destructively if the case of scopes match.
       * There are two cases:
       * 1) If we do unification just after a conde, then the scope is already incremented and nothing goes into
       *    the fresh variables.
       * 2) If we do unification after a fresh, then in case of failure it doesn't matter if
       *    the variable is be distructively substituted: we will not look on it in future.
       *)
      if scope = var.scope
      then begin
        var.subst <- Some (Obj.repr term);
        subst
      end
        else VarMap.add var (Term.repr term) subst

    exception Unification_failed

    let unify ~scope env subst x y =
      let walk = walk env subst in
      (* The idea is to do the unification and collect the unification prefix during the process *)
      let extend var term (prefix, subst) =
        let subst = extend ~scope env subst var term in
        (Binding.({var; term})::prefix, subst)
      in
      let rec helper x y acc =
        let open Term in
        fold2 x y ~init:acc
          ~fvar:(fun acc x y ->
            match walk x, walk y with
            | Var x, Var y      ->
              if Var.equal x y then acc else extend x (Term.repr y) acc
            | Var x, Value y    -> extend x y acc
            | Value x, Var y    -> extend y x acc
            | Value x, Value y  -> helper x y acc
          )
          ~fval:(fun acc t x y ->
            if Term.is_valid_tag t then
              if x = y then acc else raise Unification_failed
            else
              failwith (sprintf "OCanren fatal (Subst.unify): invalid value (%d)" t)
          )
          ~fvarval:(fun acc v t y ->
            if Term.is_valid_tag t then
              match walk v with
              | Var v    -> extend v y acc
              | Value x  -> helper x y acc
            else
              failwith (sprintf "OCanren fatal (Subst.unify): invalid value (%d)" t)
          )
      in
      let x, y = Term.(repr x, repr y) in
      try
        Some (helper x y ([], subst))
      with Term.Different_shape | Unification_failed | Occurs_check -> None

    let reify ~f env subst x = Obj.magic @@
      map env subst (Term.repr x)
        ~fvar:(fun v -> Term.repr (f v))
        ~fval:(fun t x ->
          if Term.is_valid_tag t then x
          else failwith (sprintf "OCanren fatal (Subst.reify): invalid value (%d)" t)
        )

    let project env subst x = Obj.magic @@
      map env subst (Term.repr x)
        ~fvar:(fun v -> Term.repr v)
        ~fval:(fun t x ->
          if Term.is_valid_tag t then x
          else failwith (sprintf "OCanren fatal (Subst.project): invalid value (%d)" t)
        )

    let refresh ?(mapping = VarTbl.create 31) ~scope dst_env src_env subst x =
      let rec helper x = Obj.magic @@
        map src_env subst (Term.repr x)
          ~fvar:(fun v ->
            try
              Term.repr @@ VarTbl.find mapping v
            with Not_found ->
              let new_var = Env.fresh ~scope dst_env in
              VarTbl.add mapping v !!!new_var;
              Term.repr @@ new_var
          )
          ~fval:(fun t x ->
            if Term.is_valid_tag t then x
            else failwith (sprintf "OCanren fatal (Subst.refresh): invalid value (%d)" t)
          )
      in
      mapping, helper x

    let free_vars env subst x =
      Env.free_vars env @@ project env subst x

    let is_bound = VarMap.mem

    let merge env subst1 subst2 = VarMap.fold (fun var term -> function
      | Some s  -> begin
        match unify ~scope:Var.non_local_scope env s !!!var term with
        | Some (_, s') -> Some s'
        | None         -> None
        end
      | None    -> None
    ) subst1 (Some subst2)

    (* let is_subsumed env subst =
      VarMap.for_all (fun var term ->
        match unify ~scope:Var.non_local_scope env subst !!!var term with
        | None          -> false
        | Some ([], _)  -> true
        | _             -> false
      ) *)  

  end

module Int = struct type t = int let compare = (-) end
module M = Map.Make(Int)

exception Disequality_violated
exception Disequality_fulfilled

module Disequality :
  sig
    (* Efficient representation for storing and updating disequalities during search *)
    type t

    (* Simple representation of disequalities as a formula in Conjunctive Normal Form *)
    type cnf

    (* Simple representation of disequalities as a formula in Disjunctive Normal Form *)
    type dnf = Binding.t list list

    val empty  : t

    (* [of_disj env subst] build a disequality constraint store from a list of bindings
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) \/ (y =/= 6)
     *)
    (* val of_disj : Env.t -> Binding.t list -> t *)

    (* [of_conj env subst] build a disequality constraint store from a list of bindings
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) /\ (y =/= 6)
     *)
    (* val of_conj : Env.t -> Binding.t list -> t *)

    (* [to_cnf env subst diseq] - returns new disequality in cnf representation *)
    val to_cnf : Env.t -> Subst.t -> t -> cnf

    (* [of_cnf env diseq] - builds efficient representation of disequalities from cnf representation *)
    val of_cnf : Env.t -> cnf -> t

    (* [normalize diseq] - normalizes disequalities in cnf representation,
     *   i.e. sorts them such that equal disequalities can be compared by simple equivalence check.
     *   Note that additional measures should be performed in order to do alpha-equivalence check of disequalities.
     *)
    val normalize : cnf -> cnf

    (* [cnf_to_dnf diseq] - converts disequalities in cnf representation to dnf representation *)
    val cnf_to_dnf : cnf -> dnf

    (* [of_dnf env diseq] - converts disequalities in dnf representation into a list of
     *   disequalities in efficient representation.
     *   Semantically the result list is a list of disequalities bound by disjunction.
     *)
    (* val of_dnf : Env.t -> dnf -> t list *)

    val add : Env.t -> Subst.t -> t -> 'a -> 'a -> t option

    (* [recheck env subst diseq bindings] - checks that disequality is not violated in refined substitution.
     *   [bindings] is a substitution prefix, i.e. new bindings obtained during unification.
     *   This function may rebuild internal representation of constraints and thus it returns new object.
     *)
    val recheck : Env.t -> Subst.t -> t -> Binding.t list -> t option

    (* [extend ~prefix env diseq] - extends disequality with new bindings.
     *   New bindings are interpreted as formula in DNF.
     *   Ex. [(x, 5); (y, 6)] --> (x =/= 5) \/ (y =/= 6)
     *)
    (* val extend : prefix:Binding.t list -> Env.t -> t -> t *)

    (* [merge env diseq1 diseq2] - merges two disequality stores *)
    val merge : Env.t -> t -> t -> t

    (* val reify : Env.t -> Subst.t -> t -> Var.t -> 'a list *)

    (* [project env subst diseq fv] - projects [diseq] into the set of free-variables [fv],
     *   i.e. it extracts only those constraints that mention at least one variable from [fv]
     *)
    val project : Env.t -> Subst.t -> cnf -> VarSet.t -> cnf

    (* [refresh ?mapping ~scope dst_env src_env subst diseq fv] - takes a disequality store [diseq]
     *   and replaces all free variables into fresh variables in destination environment.
     *   Returns a modified disequality store along with a mapping from variables in [src_env] into [dst_env].
     *   Takes an optional argumet [mapping], in case it is passed a variable from [src_env]
     *   first is looked up in [mapping] and only if it is not present there
     *   fresh variable from [dst_env] is allocated.
     *)
    val refresh : ?mapping: Var.t VarTbl.t -> scope:Var.scope -> Env.t -> Env.t -> Subst.t -> cnf -> Var.t VarTbl.t * cnf
  end =
  struct
    (* Disequality constraints are represented as formula in CNF
     * where each atom is single disequality
     * (i.g. ({x =/= t} \/ {y =/= u}) /\ ({y =/= v} \/ {z =/= w}))
     *
     * Optimisation:
     * For each disjunct in the formula we choose one `sample` (i.e single disequality {x =/= t}).
     * Whenever we want to check the whole disequality constraint we
     * can check single `sample` from each conjunct.
     * If `sample` check is passed (i.e {x =/= t} holds in current substitution) we can
     * skip checks of other disjuncts in this conjunct.
     * Also note that after unification of two terms we can
     * check only those disequalities that involves changed variables.
     * Because of that we maintain an index - a map from variable index to
     * list of conjuncts for which this variable is a `sample`.
     * When `sample` check fails, we change index.
     * We choose another `sample` {y =/= u} and add binding to the map for variable {y}.
     * There is no need to check previous samples in the future (because its assumption is already broken in current substitution)
    *)

    module Disjunct :
      sig
        (* Disjunction.t is a set of single disequalities joint by disjunction *)
        type t

        val of_list : Binding.t list -> t

        (* val to_list : t -> Binding.t list *)

        val make : Env.t -> Subst.t -> 'a -> 'a -> t

        (* returns an index of variable involved in some disequality inside disjunction *)
        val samplevar : t -> Binding.t

        (* [recheck env subst disj] - checks that disjunction of disequalities is
         *   not violated in (current) substitution.
         *   This function is designed to incrementally refine disequalities
         *   with a series of more and more specialized substitutions.
         *   If arbitary substitutions are passed the result may be invalid.
         *)
        val recheck : Env.t -> Subst.t -> t -> t

        val is_subsumed : Ent.t -> Subst.t -> t -> t -> bool

        (* [refine env subst disj] - returns `disequality` prefix along with substitution specialized with that prefix.
         *   It is used in two cases:
         *   1) When we want to `negate` a state of search we try to unify current substitution with disequalities.
         *        If unification succeeds we obtain a new specialized state - a counterexample.
         *        Otherwise the substitution with disequalities forms a tautology and we can skip them.
         *   2) When we want to reify an answer we again try to unify current substitution with disequalities.
         *        Then we look into `disequality` prefix for bindings that should not be met.
         *)
        val reify : Env.t -> Subst.t -> t -> (Binding.t list * Subst.t) option

        val equal : t -> t -> bool
        val compare : t -> t -> int
      end =
      struct
        type t = Term.t VarMap.t

        let update t =
          ListLabels.fold_left ~init:t
            ~f:(let open Binding in fun acc {var; term} ->
              if VarMap.mem var then
                (* in this case we have subformula of the form (x =/= t1) \/ (x =/= t2) which is always SAT *)
                raise Disequality_fulfilled
              else
                VarMap.add var term acc
            )

        let of_list = update VarMap.empty

        (* let to_list ds = ds *)

        let samplevar t = fst @@ VarMap.choose t

        type status =
          | Fulfiled
          | Violated
          | Refined of Binding.t list

        let refine env subst x y =
          match Subst.unify ~scope:Var.non_local_scope env subst x y with
          | None              -> Fulfiled
          | Some ([], _)      -> Violated
          | Some (prefix, _)  -> Refined prefix

        let make env subst x y =
          match refine env subst x y with
          | Refined delta -> of_list delta
          | Fulfiled      -> raise Disequality_fulfilled
          | Violated      -> raise Disequality_violated

        let rec recheck env subst t =
          let var, term = VarMap.min_binding t in
          let unchecked = VarMap.remove var t in
          match refine env subst !!!var term with
          | Fulfiled       -> raise Disequality_fulfilled
          | Refined delta  -> update unchecked delta
          | Violated       ->
            if VarMap.is_empty unchecked then
              raise Disequality_violated
            else
              recheck env subst unchecked

        let is_relevant env subst t fv =
          (* left those disjuncts that contains binding only for variables from [fv],
           * otherwise it's possible to pick binding (x =/= t) from disjunct for
           * variable [x] that is not in [fv],
           * assign [t] to [x] and thus fulfill disequality
           *)
           VarMap.for_all (fun var term ->
             (VarSet.mem var fv) ||
             (match Env.var env term with Some u -> VarSet.mem u fv | None -> false)
           ) t

        let freevars env subst t =
          VarMap.fold (fun _ term acc ->
            VarSet.union acc @@ Subst.freevars env subst term
          ) t fv

        (* [t'] <= [t] where (<=) is more-general ord *)
        let is_subsumed env subst t t' =
          (* we need to check that:
           * 1) [t'] is prefix of [t] (e.g. (x=/=1 \/ y=/=2) is subsumed by (x=/=1) );
           * 2) for each binding [var, term'] in [t'] a corresponding [term] in [t] is subsumed by [term']
           *    (e.g. (x=/=[1]) is subsumed by (x=/=[_.0]) );
           * example that illustrates use of both conditions: (x=/=[1] \/ y=/=2) is subsumed by (x=/=[_.0])
           *)
          S.for_all (fun var term' ->
            try
              let term = S.find var t in
              Subst.is_subsumed env subst term term'
            with Not_found -> false
          ) t'

        let reify env subst ds =
          let result = ListLabels.fold_left ds ~init:(Some [])
                ~f:(let open Binding in fun acc { var; term } ->
                    match acc with
                    | None -> None
                    | Some bs ->
                      match refine env subst !!!var term with
                      | Fulfiled       -> None
                      | Violated       -> acc
                      | Refined delta  -> Some (delta @ bs)
                )
            in
            (* We should not get empty substituion delta here,
             * because it would mean that disequality is violated.
             * But we had to detect violations during search via `check`. *)
            assert (match result with Some [] -> false | _ -> true);
            result

        let equal = VarMap.equal Term.equal
        let compare = VarMap.compare Term.compare

      end

    module Conjunct :
      sig
        type t

        (* val of_list : Disjunct.t list -> t *)

        (* val to_list : Env.t -> Subst.t -> t -> Binding.t list list *)

        val make : Env.t -> Subst.t -> 'a -> 'a -> t

        val recheck : Env.t -> Subst.t -> t -> t

        val split : t -> t VarMap.t

        val merge : t -> t -> t

        val diff : t -> t -> t
      end = struct
        module S = Set.Make(Disjunct)

        type t = S.t

        let diff = S.diff

        let merge = S.union

        let make env subst x y =
          S.singleton @@ Disjunct.make env subst x y

        let recheck env subst t =
          S.fold (fun acc disj ->
              try
                S.add (Disjunct.recheck env subst disj) acc
              with Disequality_fulfilled -> acc
          ) t S.empty

        let split t =
          S.fold (fun acc disj ->
            let var = Disjunct.samplevar disj in
            let upd = function
            | Some conj -> Some (S.add disj conj)
            | None      -> Some (S.singleton disj)
            in
            VarMap.update var upd acc
          ) t VarMap.empty

        let remove_subsumed cs =
          S.fold (fun acc disj ->
            if S.exists (Disjunct.is_subsumed env subst disj) then
              (* if new disjunct is subsumed by another then we don't add it *)
              acc
            else
              (* otherwise we should remove all disjuncts that are subsumed by newly added *)
              S.add disj @@ S.filter (fun disj' -> Disjunct.is_subsumed env disj' disj) acc
          ) cs S.empty

        let project env subst t fv =
          let rec helper fv =
            let fv', t' = S.fold (fun ((fv', conj) as acc) disj ->
              (* left those disjuncts that contains binding only for variables from [fv],
               * and obtain a set of free variables from terms mentioned in those disjuncts
               *)
              if Disjunct.is_relevant env subst disj fv then
                let fv' = VarSet.union fv' @@ Disjunct.freevars env subst disj in
                fv', S.add disj conj
            ) t (fv, S.empty)
            in
            if VarSet.equal fv fv' then t' else helper fv'
          in
          remove_subsumed @@ helper fv

      end

    type t = Conjunct.t VarMap.t

    type dnf = Binding.t list list

    let empty = VarMap.empty

    let merge = VarMap.union (fun _ c c'-> Some (Conjunct.merge c c'))

    let update conj = merge (Conjunct.split conj)

    let add env subst cstore x y =
      try
        Some (update (Conjunct.make env subst x y) cstore)
      with
        | Disequality_fulfilled -> Some cstore
        | Disequality_violated  -> None

    let recheck env subst cstore bs =
      let helper var cstore =
        try
          let conj = VarMap.find var cstore in
          let cstore = VarMap.remove var cstore in
          update (Conjunct.recheck env subst conj) cstore
        with Not_found -> cstore
      in
      try
        let cstore = ListLabels.fold_left bs ~init:cstore
          ~f:(let open Binding in fun cstore {var; term} ->
            let cstore = helper var cstore in
            match Env.var env term with
            | Some u -> helper u cstore
            | None   -> cstore
          )
        in
        Some cstore
      with Disequality_violated -> None

    let project env subst cstore fv =
      let cs = VarMap.fold (fun _ -> Conjunct.merge conj acc) cstore VarMap.empty in
      Conjunct.(split @ project env subst cs fv)

    let refresh ?(mapping = VarTbl.create 31) ~scope dst_env src_env subst cs =
      let refresh_binding =
        let open Binding in fun {var; term} ->
        let new_var =
          try
            VarTbl.find mapping var
          with Not_found ->
            let new_var = Env.fresh ~scope dst_env in
            VarTbl.add mapping var new_var;
            new_var
        in
        {var = new_var; term = snd @@ Subst.refresh ~mapping ~scope dst_env src_env subst term}
      in
      let cs' = List.map (List.map refresh_binding) cs in
      (mapping, cs')
end

module State :
  sig
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; scope : Var.scope
      }

    val empty : unit -> t

    val fresh : t -> 'a

    val new_scope : t -> t

    val unify : 'a -> 'a -> t -> t option
    val diseq : 'a -> 'a -> t -> t option

    val reify : 'a -> t -> (Env.t * 'a) list
  end = struct
    type t =
      { env   : Env.t
      ; subst : Subst.t
      ; ctrs  : Disequality.t
      ; scope : Var.scope
      }

    type reified = Env.t * Term.t

    let empty () =
      { env   = Env.empty ()
      ; subst = Subst.empty
      ; ctrs  = Disequality.empty
      ; scope = Var.new_scope ()
      }

    let env   {env} = env
    let subst {subst} = subst
    let constraints {ctrs} = ctrs
    let scope {scope} = scope

    let fresh {env; scope} = Env.fresh ~scope env

    let new_scope st = {st with scope = Var.new_scope ()}

    let unify x y ({env; subst; ctrs; scope} as st) =
        match Subst.unify ~scope env subst x y with
        | None -> None
        | Some (prefix, subst) ->
          match Disequality.recheck env subst ctrs prefix with
          | None      -> None
          | Some ctrs -> Some {st with subst; ctrs}

    let diseq x y ({env; subst; ctrs; scope} as st) =
      match Disequality.add env subst ctrs x y with
      | None      -> None
      | Some ctrs -> Some {st with ctrs}

    let merge
      {env=env1; subst=subst1; ctrs=ctrs1; scope=scope1}
      {env=env2; subst=subst2; ctrs=ctrs2; scope=scope2} =
      let env = Env.merge env1 env2 in
      match Subst.merge env subst1 subst2 with
      | None       -> None
      | Some subst -> Some
        { env; subst
        ; ctrs  = Disequality.merge env ctrs1 ctrs2
        ; scope = Var.new_scope ()
        }

    let reify x {env; subst; ctrs} =
      let rec helper cs forbidden x =
        Subst.reify env subst !!!x ~f:(fun v ->
          if List.mem v.Var.index forbidden then v
          else
            {v with Var.constraints =
              ListLabels.filter cs
                ~f:(let open Binding in fun {var; term} ->
                  if Var.equal v var then
                    match Env.var env term with
                    | Some u  -> not (List.mem u.Var.index forbidden)
                    | None    -> true
                  else false
                )
              |> List.map (
                let open Binding in fun {var; term} ->
                  helper cs (v.Var.index::forbidden) !!!term
              )
            }
        )
      in
      let fv = Subst.free_vars env subst x in
      Disequality.project env subst (Disequality.to_cnf env subst ctrs) fv
      |> Disequality.cnf_to_dnf
      |> List.map (fun cs -> env, Obj.magic (helper cs [] x))

  end

type 'a goal' = State.t -> 'a

type goal = State.t Stream.t goal'

let success st = Stream.single st
let failure _  = Stream.nil

let (===) x y st =
  match State.unify x y st with
  | Some st -> success st
  | None    -> failure st

let (=/=) x y st =
  match State.diseq x y st with
  | Some st -> success st
  | None    -> failure st

let delay g st = Stream.from_fun (fun () -> g () st)

let conj f g st = Stream.bind (f st) g
let (&&&) = conj
let (?&) gs = List.fold_right (&&&) gs success

let disj_base f g st = Stream.mplus (f st) (Stream.from_fun (fun () -> g st))

let disj f g st = let st = State.new_scope st in disj_base f g |> (fun g -> Stream.from_fun (fun () -> g st))

let (|||) = disj

let (?|) gs st =
  let st = State.new_scope st in
  let rec inner = function
  | [g]   -> g
  | g::gs -> disj_base g (inner gs)
  | [] -> failwith "Wrong argument of (?!)"
  in
  inner gs |> (fun g -> Stream.from_fun (fun () -> g st))

let conde = (?|)

let call_fresh f st =
  let x = State.fresh st in
  f x st

module Fresh =
  struct
    let succ prev f = call_fresh (fun x -> prev (f x))

    let zero  f = f
    let one   f = succ zero f
    let two   f = succ one f

    (* N.B. Manual inlining of numerals will speed-up OCanren a bit (mainly because of less memory consumption) *)
    (* let two   g = fun st ->
      let scope = State.scope st in
      let env = State.env st in
      let q = Env.fresh ~scope env in
      let r = Env.fresh ~scope env in
      g q r st *)

    let three f = succ two f
    let four  f = succ three f
    let five  f = succ four f

    let q     = one
    let qr    = two
    let qrs   = three
    let qrst  = four
    let pqrst = five
  end

(* ******************************************************************************* *)
(* ************************** Reification stuff ********************************** *)

include (struct

type ('a, 'b) injected = 'a

module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
    type ('a, 'b) t
    val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

let rec reify env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

let rec prjc of_int env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (prjc of_int env) v in of_int i cs
  | None   -> Obj.magic x

module Fmap (T : T1) =
  struct
    external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

    let rec reify r env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r env) v in Var (i, cs)
      | None   -> Value (T.fmap (r env) x)

    let rec prjc r of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r of_int env) v in of_int i cs
      | None   -> T.fmap (r env) x
  end

module Fmap2 (T : T2) =
  struct
    external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

    let rec reify r1 r2 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) x)

    let rec prjc r1 r2 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) x
  end

module Fmap3 (T : T3) =
  struct
    external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected = "%identity"

    let rec reify r1 r2 r3 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 r3 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) x)

    let rec prjc r1 r2 r3 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) (r3 env) x
end

module Fmap4 (T : T4) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                     (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x)

  let rec prjc r1 r2 r3 r4 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x
end

module Fmap5 (T : T5) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x)

  let rec prjc r1 r2 r3 r4 r5 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x
end

module Fmap6 (T : T6) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 r6 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x)

  let rec prjc r1 r2 r3 r4 r5 r6 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 r6 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x
end

end : sig
  type ('a, 'b) injected

  val reify : Env.t -> ('a, 'a logic) injected -> 'a logic

  val prjc : (int -> 'a list -> 'a) -> Env.t -> ('a, 'a logic) injected -> 'a

module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
    type ('a, 'b) t
    val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

module Fmap (T : T1) :
  sig
    val distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected

    val reify : (Env.t -> ('a,'b) injected -> 'b) -> Env.t -> ('a T.t, 'b T.t logic as 'r) injected -> 'r

    val prjc  : (Env.t -> ('a,'b) injected -> 'a) ->
      (int -> 'r list -> ('a T.t as 'r)) ->
      Env.t -> ('r, 'b T.t logic) injected -> 'r
  end

module Fmap2 (T : T2) :
  sig
    val distrib : (('a,'c) injected, ('b,'d) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected

    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> Env.t -> (('a, 'c) T.t, ('b, 'd) T.t logic as 'r) injected -> 'r

    val prjc  : (Env.t -> ('a, 'b) injected -> 'a) ->
      (Env.t -> ('c, 'd) injected -> 'c) ->
      (int -> 'r list -> (('a,'c) T.t as 'r) ) ->
      Env.t -> ('r, ('b,'d) T.t logic) injected -> 'r
  end

module Fmap3 (T : T3) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected

    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> (Env.t -> ('e, 'f) injected -> 'f) ->
                Env.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) injected -> 'r

    val prjc  : (Env.t -> ('a, 'b) injected -> 'a) ->
      (Env.t -> ('c, 'd) injected -> 'c) ->
      (Env.t -> ('e, 'f) injected -> 'e) ->
      (int -> 'r list -> 'r) ->
      Env.t -> (('a,'c,'e) T.t as 'r, ('b,'d,'f) T.t logic) injected -> 'r
  end

module Fmap4 (T : T4) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                       (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected

    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) ->
                (Env.t -> ('e, 'f) injected -> 'f) -> (Env.t -> ('g, 'h) injected -> 'h) ->
                Env.t -> (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t logic as 'r) injected -> 'r

    val prjc  :
      (Env.t -> ('a, 'b) injected -> 'a) -> (Env.t -> ('c, 'd) injected -> 'c) ->
      (Env.t -> ('e, 'f) injected -> 'e) -> (Env.t -> ('g, 'h) injected -> 'g) ->
      (int -> 'r list -> 'r) ->
      Env.t -> ('r, ('b,'d,'f,'h) T.t logic) injected -> (('a,'c,'e,'g) T.t as 'r)
  end

module Fmap5 (T : T5) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                       (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected

    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> (Env.t -> ('e, 'f) injected -> 'f) ->
                (Env.t -> ('g, 'h) injected -> 'h) -> (Env.t -> ('i, 'j) injected -> 'j) ->
                Env.t -> (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t logic as 'r) injected -> 'r

    val prjc  :
      (Env.t -> ('a, 'b) injected -> 'a) -> (Env.t -> ('c, 'd) injected -> 'c) ->
      (Env.t -> ('e, 'f) injected -> 'e) -> (Env.t -> ('g, 'h) injected -> 'g) ->
      (Env.t -> ('i, 'j) injected -> 'i) ->
      (int -> 'r list -> 'r) ->
      Env.t -> ('r, ('b,'d,'f,'h,'j) T.t logic) injected ->
      (('a,'c,'e,'g,'i) T.t as 'r)
  end

module Fmap6 (T : T6) :
  sig
    val distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                       (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected

    val reify : (Env.t -> ('a, 'b) injected -> 'b) -> (Env.t -> ('c, 'd) injected -> 'd) -> (Env.t -> ('e, 'f) injected -> 'f) ->
                (Env.t -> ('g, 'h) injected -> 'h) -> (Env.t -> ('i, 'j) injected -> 'j) -> (Env.t -> ('k, 'l) injected -> 'l) ->
                Env.t -> (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t logic as 'r) injected -> 'r

    val prjc  :
      (Env.t -> ('a, 'b) injected -> 'a) -> (Env.t -> ('c, 'd) injected -> 'c) ->
      (Env.t -> ('e, 'f) injected -> 'e) -> (Env.t -> ('g, 'h) injected -> 'g) ->
      (Env.t -> ('i, 'j) injected -> 'i) -> (Env.t -> ('k, 'l) injected -> 'k) ->
      (int -> 'r list -> 'r) ->
      Env.t -> ('r, ('b,'d,'f,'h,'j,'l) T.t logic) injected ->
      (('a,'c,'e,'g,'i,'k) T.t as 'r)
  end

end)

external lift : 'a -> ('a, 'a) injected                      = "%identity"
external inj  : ('a, 'b) injected -> ('a, 'b logic) injected = "%identity"

exception Not_a_value

let to_logic x = Value x

let from_logic = function
| Value x    -> x
| Var (_, _) -> raise Not_a_value

let (!!) x = inj (lift x)

class type ['a,'b] reified = object
  method is_open : bool
  method prj     : 'a
  method reify   : (Env.t -> ('a, 'b) injected -> 'b) -> 'b
  method prjc    : (Env.t -> ('a, 'b) injected -> 'a) -> 'a
end

let make_rr : Env.t -> ('a, 'b) injected -> ('a, 'b) reified  = fun env x ->
  object (self)
    method is_open            = Env.has_free_vars env x
    method prj                = if self#is_open then raise Not_a_value else !!!x
    method reify reifier      = reifier env x
    method prjc  onvar        = onvar   env x
  end

let prj x = let rr = make_rr (Env.empty ()) x in rr#prj

module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module Curry =
  struct
    let one = (@@)
    let succ k f x = k (fun tup -> f (x, tup))
  end

module Uncurry =
  struct
    let one = (@@)
    let succ k f (x,y) = k (f x) y
  end

module LogicAdder :
  sig
    val zero : goal -> goal
    val succ : ('a -> State.t -> 'd) -> (('e, 'f) injected -> 'a) -> State.t -> ('e, 'f) injected * 'd
  end = struct
    let zero f      = f
    let succ prev f = call_fresh (fun logic st -> (logic, prev (f logic) st))
  end

module ReifyTuple = struct
  let one x env = make_rr env x
  let succ prev (x, xs) env = (make_rr env x, prev xs env)
end

let succ n () =
  let adder, app, ext, uncurr = n () in
  (LogicAdder.succ adder, ReifyTuple.succ app, ExtractDeepest.succ ext, Uncurry.succ uncurr)

let one   () = (LogicAdder.(succ zero)), ReifyTuple.one, ExtractDeepest.ext2, Uncurry.one
let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let qrstu = five

let run n g h =
  let adder, reifier, ext, uncurr = n () in
  let args, stream = ext @@ adder g @@ State.empty () in
  Stream.bind stream (fun st -> Stream.of_list @@ State.reify args st)
  |> Stream.map (fun (env, tup) -> uncurr h @@ reifier tup env)

(** ************************************************************************* *)
(** Tabling primitives                                                        *)

module Table :
  sig
    (* Type of `answer` term.
     * Answer term is a term where all free variables are renamed to 0 ... n.
     *)
    type answer

    (* Type of table.
     * Table is a map from answer term to the set of answer terms,
     * i.e. answer -> [answer]
     *)
    type t

    val create   : unit -> t
    val abstract : 'a -> State.t -> answer

    val master : t -> answer -> 'a -> goal -> goal
    val slave  : t -> answer -> 'a -> goal
  end = struct
    type answer = Env.t * Obj.t * Disequality.cnf

    module Cache :
      sig
        type t

        val create    : unit -> t

        val add       : t -> answer -> unit
        val contains  : t -> answer -> bool
        val consume   : t -> 'a -> goal
      end =
      struct
        type t = answer list ref

        let create () = ref []

        let add cache answ =
          cache := List.cons answ !cache

        let contains cache (_, answ, diseq) =
          ListLabels.exists !cache
            ~f:(fun (_, answ', diseq') ->
              (* all variables in both terms are renamed to 0 ... n (and constraints are sorted),
               * because of that simple equivalence test is enough.
               * TODO: maybe we need [is_more_general answ' answ] test here
               * TODO: maybe there is more clever way to compare disequalities
               *)
               (answ = answ') && (diseq = diseq')
            )

        let consume cache args =
          let open State in fun {env; subst; scope} as st ->
          let st = State.new_scope st in
          let scope = st.scope in
          (* [helper iter seen] consumes answer terms from cache one by one
           *   until [iter] (i.e. current pointer into cache list) is not equal to [seen]
           *   (i.e. to the head of seen part of the cache list)
           *)
          let rec helper iter seen =
            if iter == seen then
              (* update `seen` - pointer to already seen part of cache *)
              let seen = !cache in
              (* delayed check that current head of cache is not equal to head of seen part *)
              let is_ready () = seen != !cache  in
              (* delayed thunk starts to consume unseen part of cache  *)
              Stream.suspend ~is_ready @@ fun () -> helper !cache seen
            else
              (* consume one answer term from cache *)
              let answ_env, answ_term, answ_ctrs = List.hd iter in
              let tail = List.tl iter in
              (* `lift` answer term to current environment *)
              let mapping, answ_term = Subst.refresh ~scope env answ_env Subst.empty answ_term in
              match State.unify (Obj.repr args) answ_term st with
                | Some ({subst=subst'; ctrs=ctrs'} as st') ->
                  begin
                  (* `lift` answer constraints to current environment *)
                  let _, answ_ctrs = Disequality.refresh ~mapping ~scope env answ_env Subst.empty answ_ctrs in
                  let answ_ctrs = Disequality.of_cnf env answ_ctrs in
                  (* check answ_ctrs against external substitution *)
                  match Disequality.recheck env subst' answ_ctrs (Subst.split subst) with
                  | None      -> helper tail seen
                  | Some ctrs ->
                    let f = Stream.from_fun @@ fun () -> helper tail seen in
                    Stream.cons {st' with ctrs = Disequality.merge env ctrs' ctrs} f
                  end
                | None -> helper tail seen
          in
          helper !cache []

      end

    type t = (Obj.t, Cache.t) Hashtbl.t

    let create () = Hashtbl.create 1031

    let empty_diseq =
      Disequality.to_cnf (Env.create ~anchor:Var.tabling_env) Subst.empty Disequality.empty

    let abstract args =
      let open State in fun {env; subst} ->
      let tenv = Env.create ~anchor:Var.tabling_env in
      (tenv, Obj.repr @@ snd @@ Subst.refresh ~scope:Var.tabling_scope tenv env subst args, empty_diseq)

    let extract_ctrs mapping answ_env env subst ctrs args =
      let fv = Subst.free_vars env subst args in
      let cs = Disequality.project env subst (Disequality.to_cnf env subst ctrs) fv in
      Disequality.normalize @@ snd @@ Disequality.refresh ~mapping ~scope:Var.tabling_scope answ_env env subst cs

    let extract_answer args =
      let open State in fun {env; subst; ctrs} ->
      let answ_env = Env.create ~anchor:Var.tabling_env in
      let mapping, answ_term = Subst.refresh ~scope:Var.tabling_scope answ_env env subst args in
      let answ_ctrs = extract_ctrs mapping answ_env env subst ctrs args in
      (answ_env, Obj.repr answ_term, answ_ctrs)

    let master tbl (_, k, _) args g =
      (* create new cache entry in table *)
      let cache = Cache.create () in
      Hashtbl.add tbl k cache;
      let open State in fun ({env; subst; ctrs} as st) ->
      (* This `fake` goal checks whether cache already contains new answer.
       * If not then this new answer is added to the cache.
       *)
      let hook ({env=env'; subst=subst'; ctrs=ctrs'} as st') =
        let answ = extract_answer args st' in
        if not (Cache.contains cache answ) then begin
          Cache.add cache answ;
          (* TODO: we only need to check diff, i.e. [subst' \ subst] *)
          match Disequality.recheck env subst' ctrs (Subst.split subst') with
          | Some ctrs -> success {st' with ctrs = Disequality.merge env ctrs ctrs'}
          | None      -> failure ()
        end
        else failure ()
      in
      (g &&& hook) {st with ctrs = Disequality.empty}

    let slave tbl (_, k, _) args =
      let open State in fun ({env} as st) ->
      let cache = Hashtbl.find tbl k in
      Cache.consume cache args st
  end

module Tabling =
  struct
    let succ n () =
      let currier, uncurrier = n () in
      let sc = (Curry.succ : (('a -> 'b) -> 'c) -> ((((_, _) injected as 'k) * 'a -> 'b) -> 'k -> 'c)) in
      (sc currier, Uncurry.succ uncurrier)

    let one () = ((Curry.(one) : ((_, _) injected -> _) as 'x -> 'x), Uncurry.one)

    let two   () = succ one ()
    let three () = succ two ()
    let four  () = succ three ()
    let five  () = succ four ()

    let tabled' tbl g args st =
      let key = Table.abstract args st in
      try
        Table.slave tbl key args st
      with Not_found ->
        Table.master tbl key args (g args) st

    let tabled n g =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      currier (fun tup ->
        tabled' tbl (uncurrier g) tup
      )

    let tabledrec n g_norec =
      let tbl = Table.create () in
      let currier, uncurrier = n () in
      let g = ref (fun _ -> assert false) in
      let g_rec args = uncurrier (g_norec !g) args in
      let g_tabled = tabled' tbl g_rec in
      g := currier (fun tup ->
        g_tabled tup
      );
      !g
end

(* Tracing/debugging stuff *)

(* module State

    let show  (env, subst, constr, scp) =
      sprintf "st {%s, %s} scope=%d" (Subst.show subst) (Constraints.show ~env constr) scp
*)

(* module Subst
    let show m =
      let b = Buffer.create 40 in
      Buffer.add_string b "subst {\n";
      M.iter (fun i {new_val} -> bprintf b "  %d -> %s;\n" i (generic_show new_val)) m;
      Buffer.add_string b "}";
      Buffer.contents b
=======


let unify ?p (x: _ injected) y =
  Trace.(trace two @@ unif ?p) x y (
    let open State in fun {env; subst; ctrs; scope} as st ->
    match Subst.unify env x y scope subst with
    | None -> failure ~reason:"Unification failed" st
    | Some (prefix, s) ->
        try
          let ctrs' = Constraints.check ~prefix env s ctrs in
          success {st with subst=s; ctrs=ctrs'}
        with Disequality_violated ->
          failure ~reason:"Disequality constraints violated" st
  )

let (===) x y = unify x y

let diseq ?p x y =
  Trace.(trace two @@ diseq ?p) x y (
    let open State in fun {env; subst; ctrs; scope} as st ->
    (* For disequalities we unify in non-local scope to prevent defiling *)
    match Subst.unify env x y non_local_scope subst with
    | None -> success st
    | Some ([],_) -> failure ~reason:"Constraint cannot be fulfilled" st
    | Some (prefix,_) ->
        let ctrs' = Constraints.extend ~prefix env ctrs in
        success {st with ctrs=ctrs'}
  )

let (=/=) x y = diseq x y

let delay : (unit -> goal) -> goal = fun g ->
  fun st -> MKStream.from_fun (fun () -> g () st)
>>>>>>> 81053c4... WIP: tabling

    let pretty_show is_var m =
      let b = Buffer.create 40 in
      bprintf b "subst {\n";
      M.iter (fun i {new_val} -> bprintf b "  %d -> %s;\n" i (pretty_generic_show is_var new_val)) m;
      bprintf b "}";
      Buffer.contents b
*)

(* module Constraints

  let bprintf_single ~env b cs =
    let rec helper = function
    | [] -> ()
    | c :: tl ->
          bprintf b "%d -> %s;" (!!!c.Subst.var : Var.t).index (pretty_generic_show (Env.is_var env) c.Subst.term);
          helper tl
    in
    helper cs

  let show_single ~env (c: single) =
    let b = Buffer.create 40 in
    bprintf b " ( ";
    let () = bprintf_single ~env b c in
    bprintf b " ) ";
    Buffer.contents b

  let show ~env cstore =
    let b = Buffer.create 40 in
    M.iter (fun k css ->
      bprintf b "\t%d -> [ " k;
      List.iter (fun s -> bprintf_single ~env b s) css;
      bprintf b " ]\n";
    ) cstore;
    Buffer.contents b

*)


(*
let generic_show ?(maxdepth=99999) x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner depth o =
    if depth > maxdepth then Buffer.add_string b "..." else
      match wrap o with
      | Invalid n                                         -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
      | Unboxed s when Obj.(string_tag = (tag @@ repr s)) -> bprintf b "\"%s\"" (!!!s)
      | Unboxed n when !!!n = 0                           -> Buffer.add_string b "[]"
      | Unboxed n                                         -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
      | Boxed  (t, l, f) ->
          Buffer.add_string b (Printf.sprintf "boxed %d <" t);
          for i = 0 to l - 1 do (inner (depth+1) (f i); if i<l-1 then Buffer.add_string b " ") done;
          Buffer.add_string b ">"
  in
  inner 0 x;
  Buffer.contents b

(* TODO *)
let pretty_generic_show ?(maxdepth= 99999) is_var x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner depth term =
    if depth > maxdepth then Buffer.add_string b "..." else
    if is_var !!!term then begin
      let var = (!!!term : Var.t) in
      match var.subst with
      | Some term ->
          bprintf b "{ _.%d with subst=" var.index;
          inner (depth+1) term;
          bprintf b " }"
      | None -> bprintf b "_.%d" var.index
    end else match wrap term with
      | Invalid n                                         -> bprintf b "<invalid %d>" n
      | Unboxed s when Obj.(string_tag = (tag @@ repr s)) -> bprintf b "\"%s\"" (!!!s)
      | Unboxed n when !!!n = 0                           -> Buffer.add_string b "[]"
      | Unboxed n                                         -> bprintf b  "int<%d>" (!!!n)
      | Boxed  (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (depth+1) (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner 0 x;
  Buffer.contents b

let trace msg g = fun state ->
  printf "%s: %s\n%!" msg (State.show state);
  g state

let reify_with_state (env,subs,cs,_) term = reify' env subs (Constraints.reify env subs cs) (Obj.repr term)

let project1 ~msg : (helper -> 'b -> string) -> ('a, 'b) injected -> goal = fun shower q st ->
  printf "%s %s\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q);
  success st

let project2 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> goal = fun shower q r st ->
  printf "%s '%s' and '%s'\n%!" msg (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q)
                                    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st r);
  success st

let project3 ~msg : (helper -> 'b -> string) -> (('a, 'b) injected as 'v) -> 'v -> 'v -> goal = fun shower q r s st ->
  printf "%s '%s' and '%s' and '%s'\n%!" msg
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st q)
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st r)
    (shower (helper_of_state st) @@ Obj.magic @@ reify_with_state st s);
  success st

let unitrace ?loc shower x y = fun st ->
  incr logged_unif_counter;

  let ans = (x === y) st in
  (* printf "%d: unify '%s' and '%s'" !logged_unif_counter (shower (helper_of_state st) x) (shower (helper_of_state st) y);
  (match loc with Some l -> printf " on %s" l | None -> ());

  if Stream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans

let diseqtrace shower x y = fun st ->
  incr logged_diseq_counter;
  let ans = (x =/= y) st in
  (* printf "%d: (=/=) '%s' and '%s'" !logged_diseq_counter
    (shower (helper_of_state st) x)
    (shower (helper_of_state st) y);
  if Stream.is_nil ans then printfn "  -"
  else  printfn "  +"; *)
  ans;;

(* ***************************** a la relational StdLib here ***************  *)

let report_counters () = ()
*)
