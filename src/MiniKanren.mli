(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, 
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

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

(** {3 Lazy streams} *)

module Stream :
  sig

    (** A type of the stream *)
    type 'a t

    (** Emptiness test *)
    val is_empty : 'a t -> bool

    (** Constructor *)
    val from_fun : (unit -> 'a t) -> 'a t

    (** [retrieve ~n:n s] returns the list of [n]-first elements of [s] and the rest of the stream *)
    val retrieve : ?n:int -> 'a t -> 'a list * 'a t

    (** [take ~n:n s] returns the list of [n]-first elements of [s] *)
    val take : ?n:int -> 'a t -> 'a list

    (** [hd s] gets a head of the stream *)
    val hd : 'a t -> 'a

    (** [hd s] gets a tail of the stream *)
    val tl : 'a t -> 'a t
    
    (** [map f s] maps function [f] over the stream [s] *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [iter f s] iterates function [f] over the stream [s] *)
    val iter : ('a -> unit) -> 'a t -> unit

    (** [zip s s'] returns the stream of pairs where first element is taken from [s] and second from [s'];
        raises Invalid_argument if the two streams have different lengths *)
    val zip : 'a t -> 'b t -> ('a * 'b) t 
  end

(** {3 Logics} *)

(** A type of abstract logic values *)
type 'a logic

(** A GT-compatible typeinfo for ['a logic] *)
val logic :
  (unit, 
   < show    : ('a -> string) -> 'a logic -> string;    
     html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
     eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
     compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
     foldl   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     foldr   : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
     gmap    : ('a -> 'sa) -> 'a logic -> 'sa logic 
   >) GT.t

(** Injecting values into logics *)
val (!!) : 'a -> 'a logic

(** A synonym for [(!!)] *)
val inj : 'a -> 'a logic 

(** Exception to raise on a non-value case *)
exception Not_a_value

exception Not_printable

(** Projecting logics to values (partial, can raise [Not_a_value]) *)
val (!?) : 'a logic -> 'a

(** A synonym for [(!?)] *)
val prj : 'a logic -> 'a

(** Projection with failure continuation; [prj_k k l] calls continuation [k],
    when a free variable is encountered inside [l]; the number of variable is
    passed to [k] as its argument *)
val prj_k : (int -> 'a logic list -> 'a) -> 'a logic -> 'a

(** {3 Support for some predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
@type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, html, eq, compare, foldl, foldr, gmap

(** Abstract nat type *)
@type 'a lnat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap

(** {3 States and goals} *)

(** A state *)
module State :
  sig
    (** State type *)
    type t

    (** Printing helper *)
    val show : t -> string
  end

module LogEntry :
  sig
    type uresult = Succ | Fail | Violation

    type t =
    [ `Unification of string * string * uresult
    | `Fresh       of string
    | `Label       of string ]
  end

module type Logger = 
  sig
    type t
    
    val empty : unit -> t
    val log : LogEntry.t -> t -> t
  end

module EmptyLogger : Logger

module Make (Log : Logger) : 
  sig

    type state

    (** Goal converts a state into lazy stream of states *)
    type goal = state -> state Stream.t

    module Bool :
      sig

        (** Type synonym to prevent toplevel [logic] from being hidden *)
        type 'a logic' = 'a logic 

        (** Ground boolean (the regular one) *)
        type ground = bool

        (** GT-compatible typeinfo for [ground] *)
        val ground :
          (unit,
           < compare : ground -> ground -> GT.comparison;
             eq      : ground -> ground -> bool;
             foldl   : 'a -> ground -> 'a;
             foldr   : 'a -> ground -> 'a;
             gmap    : ground -> ground;
             html    : ground -> HTML.viewer;
             show    : ground -> string >)
          GT.t 

        (** Logic boolean *)
        type logic = bool logic'

        (** GT-compatible typeinfo for [logic] *)
        val logic :
          (unit,
           < compare : logic -> logic -> GT.comparison;
             eq      : logic -> logic -> bool;
             foldl   : 'a -> logic -> 'a;
             foldr   : 'a -> logic -> 'a;
             gmap    : logic -> logic;
             html    : logic -> HTML.viewer;
             show    : logic -> string >)
          GT.t

        (** Sheffer stroke *)
        val (|^) : logic -> logic -> logic -> goal

        (** Negation *)
        val noto' : logic -> logic -> goal

        (** Negation as a goal *)
        val noto : logic -> goal

        (** Disjunction *)
        val oro : logic -> logic -> logic -> goal 

        (** Disjunction as a goal *)
        val (||) : logic -> logic -> goal

        (** Conjunction *)
        val ando : logic -> logic -> logic -> goal

        (** Conjunction as a goal *)
        val (&&) : logic -> logic -> goal

      end

    module Nat :
      sig

        (** Type synonym to prevent toplevel [logic] from being hidden *)
        type 'a logic' = 'a logic 

        (** Synonym for abstract nat type *)
        type 'a t = 'a lnat

        (** Ground nat are ismorphic for regular one *)
        type ground = ground t

        (** GT-compatible typeinfo for [ground] *)
        val ground :
          (unit,
           < compare : ground -> ground -> GT.comparison;
             eq      : ground -> ground -> bool;
             foldl   : 'a -> ground -> 'a;
             foldr   : 'a -> ground -> 'a;
             gmap    : ground -> ground;
             html    : ground -> HTML.viewer;
             show    : ground -> string >)
          GT.t

        (** Logic nat *)
        type logic = logic t logic'

        (** GT-compatible typeinfo for [logic] *)
        val logic :
          (unit,
           < compare : logic -> logic -> GT.comparison;
             eq      : logic -> logic -> bool;
             foldl   : 'a -> logic -> 'a;
             foldr   : 'a -> logic -> 'a;
             gmap    : logic -> logic;
             html    : logic -> HTML.viewer;
             show    : logic -> string >)
          GT.t

        (** [of_int n] converts integer [n] into [ground]; negtive
            integers become [O] *)
        val of_int : int -> ground

        (** [to_int g] converts ground [n] into integer *)
        val to_int : ground -> int

        (** [inj n] converts ground nat [n] into logic one *)
        val inj : ground -> logic

        (** Projection with failure continuation *)
        val prj_k : (int -> logic list -> logic t) -> logic -> ground

        (** Projection with default continuation *)
        val prj : logic -> ground

        (** Relational addition *)
        val addo : logic -> logic -> logic -> goal 

        (** Infix syninym for [addo] *)
        val (+) : logic -> logic -> logic -> goal

        (** Relational multiplication *)
        val mulo : logic -> logic -> logic -> goal 

        (** Infix syninym for [mulo] *)
        val ( + ) : logic -> logic -> logic -> goal

        (** Comparisons *)
        val leo : logic -> logic -> Bool.logic -> goal
        val geo : logic -> logic -> Bool.logic -> goal
        val gto : logic -> logic -> Bool.logic -> goal
        val lto : logic -> logic -> Bool.logic -> goal

        (** Comparisons as goals *)
        val (<=) : logic -> logic -> goal
        val (>=) : logic -> logic -> goal
        val (>)  : logic -> logic -> goal
        val (<)  : logic -> logic -> goal

      end

    (** [inj_nat n] is a deforested synonym for injection *)
    val inj_nat : int -> Nat.logic

    (** [prj_nat n] is a deforested synonym for projection *)
    val prj_nat : Nat.logic -> int

    module List :
      sig

        (** {3 Standard list definitions} *)
        include module type of struct include List end

        (** Type synonym to prevent toplevel [logic] from being hidden *)
        type 'a logic' = 'a logic 

        (** Synonym for abstract list type *)
        type ('a, 'l) t = ('a, 'l) llist

        (** Ground lists (isomorphic to regular ones) *)
        type 'a ground = ('a, 'a ground) t

        (** GT-compatible typeinfo for ['a ground] *)
        val ground :
          (unit,
           < compare : ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground -> GT.comparison;
             eq      : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
             foldl   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
             foldr   : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
             gmap    : ('a -> 'b) -> 'a ground -> 'b ground;
             html    : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
             show    : ('a -> string) -> 'a ground -> string >)
          GT.t

        (** [of_list l] makes ground list from a regular one *)
        val of_list : 'a list -> 'a ground

        (** [to_list l] make regular list from a ground one *)
        val to_list : 'a ground -> 'a list

        (** Logic lists (with the tails as logic lists) *)
        type 'a logic  = ('a, 'a logic)  t logic'

        (** GT-compatible typeinfo for ['a logic] *)
        val logic :
          (unit,
           < compare : ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison; 
             eq      : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool; 
             foldr   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
             foldl   : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b; 
             gmap    : ('a -> 'b) -> 'a logic -> 'b logic;
             html    : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
             show    : ('a -> string) -> 'a logic -> GT.string >)
          GT.t 

        (** List injection *)
        val inj : ('a -> 'b) -> 'a ground -> 'b logic

        (** List projection with failure continuation *)
        val prj_k : ('a -> 'b) -> (int -> 'a logic list -> ('a, 'a logic) t) ->  'a logic -> 'b ground

        (** List projection with default continuation *)
        val prj : ('a -> 'b) -> 'a logic -> 'b ground

        (** Relational foldr *)
        val foldro : ('a logic' -> 'b logic' -> 'b logic' -> goal) -> 'b logic' -> 'a logic' logic -> 'b logic' ->  goal

        (** Relational map *)
        val mapo : ('a logic' -> 'b logic' -> goal) -> 'a logic' logic ->'b logic' logic -> goal

        (** Relational filter *)
        val filtero : ('a logic' -> Bool.logic -> goal) -> 'a logic' logic ->'a logic' logic -> goal

        (** Relational lookup *)
        val lookupo : ('a logic' -> Bool.logic -> goal) ->  'a logic' logic -> 'a logic' option logic' -> goal

        (** Boolean list disjunctions *)
        val anyo : Bool.logic logic -> Bool.logic -> goal

        (** Boolean list conjunction *)
        val allo : Bool.logic logic -> Bool.logic -> goal

        (** Relational length *)
        val lengtho : 'a logic' logic -> Nat.logic -> goal

        (** Relational append *)
        val appendo : 'a logic' logic -> 'a logic' logic -> 'a logic' logic -> goal

        (** Relational reverse *)
        val reverso : 'a logic' logic -> 'a logic' logic -> goal

        (** Relational occurrence check (a shortcut) *)
        val membero : 'a logic' logic -> 'a logic' -> goal

      end

    (** [inj_list l] is a deforested synonym for injection *)
    val inj_list : 'a list -> 'a logic List.logic

    (** [prj_list] is a deforested synonym for projection *)
    val prj_list : 'a logic List.logic -> 'a list

    (** [inj_nat_list l] is a deforsted synonym for injection *)
    val inj_nat_list : int list -> Nat.logic List.logic

    (** [inj_nat_list l] is a deforsted synonym for projection *)
    val prj_nat_list : Nat.logic List.logic -> int list

    (** Infix synonym for [Cons] *)
    val (%) : 'a -> 'a List.logic -> 'a List.logic

    (** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
    val (%<) : 'a -> 'a -> 'a List.logic

    (** [!< x] is a synonym for [Cons (x, !Nil)] *)
    val (!<) : 'a -> 'a List.logic

    (** [nil] is a synonym for [inj Nil] *)
    val nil : 'a List.logic

    (** {2 miniKanren basic primitives} *)

    (** [msg <=> s] utility function for logging *)
    val (<=>) : string -> goal -> goal

    (** [call_fresh f] creates a fresh logical variable and passes it to the
        parameter *)
    val call_fresh : ('a logic -> state -> 'b) -> state -> 'b

    (** [x === y] creates a goal, which performs a unifications of
        [x] and [y] *)
    val (===) : 'a logic -> 'a logic -> goal

    (** [x =/= y] creates a goal, which introduces a disequality constraint for
        [x] and [y] *)
    val (=/=) : 'a logic -> 'a logic -> goal

    (** Equality as boolean relation *)
    val eqo : 'a logic -> 'a logic -> Bool.logic -> goal

    (** Disequality as boolean relation *)
    val neqo : 'a logic -> 'a logic -> Bool.logic -> goal

    (** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
    val conj : goal -> goal -> goal

    (** [&&&] is left-associative infix synonym for [conj] *)
    val (&&&) : goal -> goal -> goal

    (** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
    val disj : goal -> goal -> goal

    (** [|||] is left-associative infix synonym for [disj] *)
    val (|||) : goal -> goal -> goal

    (** [?| [s1; s2; ...; sk]] calculates [s1 ||| s2 ||| ... ||| sk] for a
        non-empty list of goals *)
    val (?|) : goal list -> goal

    (** [conde] is a synonym for [?|] *)
    val conde : goal list -> goal

    (** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
        non-empty list of goals *)
    val (?&) : goal list -> goal

    (** {2 Some predefined goals} *)

    (** [success] always succeeds *)
    val success : goal

    (** [failure] always fails *)
    val failure : goal

    (** {2 Combinators to produce fresh variables} *)
    module Fresh :
      sig

        (** [succ num f] increments the number of free logic variables in
            a goal; can be used to get rid of ``fresh'' syntax extension *)
        val succ : ('a -> state -> 'b) -> ('c logic -> 'a) -> state -> 'b

        (** Zero logic parameters *)
        val zero : 'a -> 'a

        (** {3 One to five logic parameter(s)} *)

        val one   : ('a logic ->                                                 state -> 'b) -> state -> 'b
        val two   : ('a logic -> 'b logic ->                                     state -> 'c) -> state -> 'c
        val three : ('a logic -> 'b logic -> 'c logic ->                         state -> 'd) -> state -> 'd
        val four  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'e) -> state -> 'e
        val five  : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'f

        (** {3 One to five logic parameter(s), conventional names} *)

        val q     : ('a logic ->                                                 state -> 'b) -> state -> 'b
        val qr    : ('a logic -> 'b logic ->                                     state -> 'c) -> state -> 'c
        val qrs   : ('a logic -> 'b logic -> 'c logic ->                         state -> 'd) -> state -> 'd
        val qrst  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'e) -> state -> 'e
        val pqrst : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'f

      end

    (** {2 Top-level running primitives} *)

    (** [run n g h] runs a goal [g] with [n] logical parameters and passes refined
        results to the handler [h]. The number of parameters is encoded using variadic
        machinery {a la} Danvy and represented by a number of predefined numerals and
        successor function (see below). The refinement replaces each variable, passed
        to [g], with the stream of values, associated with that variables as the goal
        succeeds. 

        Examples:

        - [run one        (fun q   -> q === !5)              (fun qs    -> ]{i here [q]s     --- a stream of all values, associated with the variable [q]}[)]
        - [run two        (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively}[)]
        - [run (succ one) (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i the same as the above}[)]
     *)
    val run : (unit -> ('a -> state -> 'c) * ('d -> 'e -> 'f) * (('g -> 'h -> 'e) * ('c -> 'h * 'g))) -> 'a -> 'd -> 'f

    (** Some type to refine a stream of states into the stream of answers (w.r.t. some known
        logic variable *)
    type 'a refiner = state Stream.t -> 'a logic Stream.t

    (** Successor function *)
    val succ :
      (unit -> ('a -> state -> 'b) * ('c -> 'd -> 'e) * (('f -> 'g -> 'h) * ('i -> 'j * 'k))) -> 
      (unit -> (('l logic -> 'a) -> state -> 'l refiner * 'b) * (('m -> 'c) -> 'm * 'd -> 'e) * (('f -> ('f -> 'n) * 'g -> 'n * 'h) * ('o * 'i -> ('o * 'j) * 'k)))

    (** {3 Predefined numerals (one to five)} *)

    val one :
      unit ->
      (('a logic -> state -> 'b) -> state -> 'a refiner * 'b) *
      (('c -> 'd) -> 'c -> 'd) * 
      (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

    val two :
      unit ->
      (('b logic -> 'c logic -> state -> 'd) -> state -> 'b refiner * ('c refiner * 'd)) *
      (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
      (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))

    val three :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> state -> 'e) -> state -> 'b refiner * ('c refiner * ('d refiner * 'e))) *
      (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
      (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))

    val four :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'b refiner * ('c refiner * ('d refiner * ('e refiner * 'f)))) *
      (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
      (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))

    val five :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> state -> 'g) -> state ->	'b refiner * ('c refiner * ('d refiner * ('e refiner * ('f refiner * 'g))))) *
      (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
      (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))

    (** {3 The same numerals with conventional names} *)

    val q :
      unit ->
      (('a logic -> state -> 'b) -> state -> 'a refiner * 'b) *
      (('c -> 'd) -> 'c -> 'd) * 
      (('e -> ('e -> 'f) -> 'f) * ('g -> 'g))

    val qr :
      unit ->
      (('b logic -> 'c logic -> state -> 'd) -> state -> 'b refiner * ('c refiner * 'd)) *
      (('e -> 'f -> 'g) -> 'e * 'f -> 'g) * 
      (('h -> ('h -> 'i) * ('h -> 'j) -> 'i * 'j) * ('k * ('l * 'm) -> ('k * 'l) * 'm))

    val qrs :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> state -> 'e) -> state -> 'b refiner * ('c refiner * ('d refiner * 'e))) *
      (('f -> 'g -> 'h -> 'i) -> 'f * ('g * 'h) -> 'i) *
      (('j -> ('j -> 'k) * (('j -> 'l) * ('j -> 'm)) -> 'k * ('l * 'm)) * ('n * ('o * ('p * 'q)) -> ('n * ('o * 'p)) * 'q))

    val qrst :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'b refiner * ('c refiner * ('d refiner * ('e refiner * 'f)))) *
      (('g -> 'h -> 'i -> 'j -> 'k) -> 'g * ('h * ('i * 'j)) -> 'k) *
      (('l -> ('l -> 'm) * (('l -> 'n) * (('l -> 'o) * ('l -> 'p))) -> 'm * ('n * ('o * 'p))) * ('q * ('r * ('s * ('t * 'u))) -> ('q * ('r * ('s * 't))) * 'u))

    val pqrst :
      unit ->
      (('b logic -> 'c logic -> 'd logic -> 'e logic -> 'f logic -> state -> 'g) -> state ->	'b refiner * ('c refiner * ('d refiner * ('e refiner * ('f refiner * 'g))))) *
      (('h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'h * ('i * ('j * ('k * 'l))) -> 'm) * 
      (('n -> ('n -> 'o) * (('n -> 'p) * (('n -> 'q) * (('n -> 'r) * ('n -> 's)))) -> 'o * ('p * ('q * ('r * 's)))) * ('t * ('u * ('v * ('w * ('x * 'y)))) -> ('t * ('u * ('v * ('w * 'x)))) * 'y))
  
  end

include (module type of Make(EmptyLogger))
