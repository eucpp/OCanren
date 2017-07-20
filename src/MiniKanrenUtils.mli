open MiniKanrenCore

module TreeLogger :
  sig
    (** Tree logger - builds a search tree *)
    class type t = object
      (** [init id] initializes tree *)
      method init: StateId.t -> unit

      (** [on_event e parentId id ] creates new node in the tree *)
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit

      (** [print ~filter formatter] pretty-prints search tree using given [formatter].
          [filter] is an optional function argument that can be used to select only interesting nodes *)
      method print: ?filter:(Listener.event -> bool) -> Format.formatter -> unit
    end

    val create : unit -> t
  end
