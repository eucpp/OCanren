open MiniKanrenCore

module TreeLogger :
  sig
    (** Tree logger - builds a search tree *)
    class type t = object
      (** [init id] initializes tree *)
      method init: StateId.t -> unit

      (** [on_event e parentId id ] creates new node in the tree *)
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit

      (** [fold f] folds a tree in a bottom-up manner using given function [f] *)
      method fold : 'a. (StateId.t * Listener.event -> 'a list -> 'a) -> 'a

      (** [print ~filter formatter] pretty-prints search tree using given [formatter].
          [filter] is an optional function argument that can be used to select only interesting nodes;
            when a goal-node is encountered in the tree, name of the goal is passed to the filter function;
            it must return true if the node should be printed and false otherwise.
          [show_unif] when true (by default) individual unifications and disequality constraint nodes are printed.
          [color] when true (by default) the output is colored *)
      method print: ?filter:(string -> bool) -> ?show_unif:bool -> ?color:bool -> Format.formatter -> unit
    end

    val create : unit -> t
  end
