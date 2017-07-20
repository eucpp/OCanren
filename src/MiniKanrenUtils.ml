open MiniKanrenCore

module TreeLogger =
  struct
    type node = { event : Listener.event; children : StateId.t list }

    let make_node e = { event = e; children = [] }

    module H = Hashtbl.Make(StateId)

    class type t = object
      method init: StateId.t -> unit
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit
      method print: ?filter:(Listener.event -> bool) -> Format.formatter -> unit
    end

    let create () = object
      val tbl : node H.t = H.create 31
      val root : StateId.t option ref = ref None

      method init id =
        (* Printf.printf "%s %s\n" (Listener.string_of_event @@ Listener.Custom "root") (StateId.show id); *)
        H.reset tbl;
        root := Some id;
        H.add tbl id @@ make_node (Listener.Custom "root")

      (** [on_event e parentId id ] creates new node in the tree *)
      method on_event e pid id =
        (* Printf.printf "%s %s %s\n" (Listener.string_of_event e) (StateId.show pid) (StateId.show id); *)
        let node = H.add tbl id @@ make_node e in
        let parent = H.find tbl pid in
        H.replace tbl pid { parent with children = id :: parent.children }

      method print ?(filter = fun _ -> true) ff =
        let rec helper {event; children} =
          let print_child childId =
            Format.fprintf ff "@;";
            helper @@ H.find tbl childId
          in
          let print_children = function
          | [] -> ()
          | children ->
            (* Format.fprintf ff "@[<v 2>"; *)
            List.iter print_child children
            (* Format.fprintf ff "@]" *)
          in
          Format.fprintf ff "@[<v 2>";
          begin
            if filter event then
            Format.fprintf ff "%s" @@ Listener.string_of_event event
            else ()
          end;
          print_children @@ List.rev children;
          Format.fprintf ff "@]"
        in
        match !root with
        | Some root -> helper @@ H.find tbl root; Format.fprintf ff "@;";
        | None -> ()
    end
  end
