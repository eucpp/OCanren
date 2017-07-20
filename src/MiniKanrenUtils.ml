open MiniKanrenCore

module TreeLogger =
  struct
    type node = { event : Listener.event; children : StateId.t list }

    module H = Hashtbl.Make(StateId)

    let make_node e = { event = e; children = [] }

    let default_filter = Listener.(function
      | Custom str -> not (String.equal "root" str)
      | Success | Failure _ -> false
      | _ -> true
    )

    let is_simple_conj tbl {event; children} = Listener.(
      let are_simple_children = List.for_all (fun id ->
        match (H.find tbl id).event with
        | Unif _  -> true
        | Diseq _ -> true
        | _ -> false
      ) in
      match event with
      | Conj -> (List.length children > 0) && (are_simple_children children)
      | _    -> false
    )

    class type t = object
      method init: StateId.t -> unit
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit
      method print: ?filter:(Listener.event -> bool) -> Format.formatter -> unit
    end

    let create () = object (self)
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

      method print ?(filter = default_filter) ff =
        let rec helper ?(break=false) ({event; children} as node) =
          let print_child ?(break=false) childId =
            helper ~break @@ H.find tbl childId
          in
          let print_children ?(break=false) = function
          | []    -> ()
          | x::xs ->
            (* Format.fprintf ff "@[<v 2>"; *)
            print_child ~break x;
            List.iter (print_child ~break:true) xs
            (* Format.fprintf ff "@]" *)
          in
          let print_conjuncts conjs =
            let children = List.fold_right (fun {children} acc -> (List.rev children) @ acc) conjs [] in
            let x::xs = List.map (fun {event} -> Listener.string_of_event event) conjs in
            Format.fprintf ff "(%s)" x;
            List.iter (fun x -> Format.fprintf ff " &&& (%s)" x) xs;
            print_children ~break:true children
          in
          if filter event then begin
            if break then Format.fprintf ff "@;" else ();
            if (is_simple_conj tbl node) then
              print_conjuncts @@ List.map (fun id -> H.find tbl id) children
            else begin
              Format.fprintf ff "@[<v 2>%s" @@ Listener.string_of_event event;
              print_children ~break:true @@ List.rev children;
              Format.fprintf ff "@]"
              end
            end
          else begin
            print_children ~break @@ List.rev children
            end
        in
        match !root with
        | Some root -> helper @@ H.find tbl root; Format.fprintf ff "@;";
        | None -> ()
    end
  end
