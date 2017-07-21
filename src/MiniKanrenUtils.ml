open MiniKanrenCore

module TreeLogger =
  struct
    type color = Green | Yellow | Red | White

    type node = { event : Listener.event; children : StateId.t list }

    module H = Hashtbl.Make(StateId)

    let make_node e = { event = e; children = []; }

    let default_filter = Listener.(function
      | Custom str -> not (String.equal "root" str)
      | Success | Failure _ -> false
      | _ -> true
    )

    let color_of_event event cs = Listener.(
      match event with
      | Success     -> Green
      | Failure _   -> Red
      | Conj        ->
        if (cs = []) then Yellow
        else if List.for_all ((==) Green) cs then Green
        else if List.exists ((==) Red) cs then Red
        else if List.exists ((==) Yellow) cs then Yellow
        else White
      | Disj        ->
        if (cs = []) then Yellow
        else if List.for_all ((==) Red) cs then Red
        else if List.exists ((==) Green) cs then Green
        else if List.exists ((==) Yellow) cs then Yellow
        else White
      | Unif _      -> let [c] = cs in c
      | Diseq _     -> let [c] = cs in c
      | Goal (_, _) -> let [c] = cs in c
      | Answer _    -> Green
      | Custom _    -> White
    )

    let color_str c str =
      if c <> White then
        let code = match c with
          | Red     -> 31
          | Green   -> 32
          | Yellow  -> 33
        in
        Printf.sprintf "\027[%dm%s\027[0m" code str
      else
        str

    type pnode = { str: string; pchildren: pnode list }

    let is_leaf {pchildren} = (pchildren == [])

    let is_foldable ps = List.for_all is_leaf ps

    let pnode_of_event event c ps = Listener.(
      let fc = color_str c in
      match event with
      | Conj when ps = [] ->
        (c, [])
      | Disj when ps = [] ->
        (c, [])
      | Conj when is_foldable ps ->
        let str = (fc "(") ^ (String.concat (fc ") &&& (") @@ List.map (fun {str} -> str) ps) ^ (fc ")") in
        (c, [{ str = str; pchildren = [] }])
      | Disj when is_foldable ps ->
        let str = (fc "(") ^ (String.concat (fc ") ||| (") @@ List.map (fun {str} -> str) ps) ^ (fc ")") in
        (c, [{ str = str; pchildren = [] }])
      | _ -> (c, [{str = color_str c @@ string_of_event event; pchildren = ps }])
    )

    let to_ptree : (Listener.event -> bool) -> Listener.event -> (color * (pnode list)) list -> color * (pnode list) =
      fun filter event children ->
        let colors = List.map fst children in
        let ps = List.concat @@ List.map snd children in
        let c = color_of_event event colors in
        if (filter event) then
          pnode_of_event event c ps
        else
          (c, ps)

    let rec print_ptree ff roots =
      let rec helper {str; pchildren} =
        Format.fprintf ff "@[<v 2>%s" str;
        print_children pchildren;
        Format.fprintf ff "@]"
      and print_children = function
        | [] -> (*Format.fprintf ff "@;"*) ()
        | xs -> List.iter (fun x -> Format.fprintf ff "@;"; helper x) xs;
      in
      print_children roots;
      Format.fprintf ff "@;"

    (* let is_simple_conj tbl {event; children} = Listener.(
      let are_simple_children = List.for_all (fun id ->
        match (H.find tbl id).event with
        | Unif _  -> true
        | Diseq _ -> true
        | _ -> false
      ) in
      match event with
      | Conj -> (List.length children > 0) && (are_simple_children children)
      | _    -> false
    ) *)

    class type t = object
      method init: StateId.t -> unit
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit
      method fold : 'a. (Listener.event -> 'a list -> 'a) -> 'a
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

      method fold : 'a. (Listener.event -> 'a list -> 'a) -> 'a = fun f ->
        let rec helper id =
          let {event; children} = H.find tbl id in
          f event @@ List.map helper (List.rev children)
        in
        match !root with
        | Some root -> helper root
        | None -> raise (Invalid_argument "Empty tree")

      method print ?(filter = default_filter) ff =
        match !root with
        | Some root ->
          let _,proots = self#fold (to_ptree filter) in
          print_ptree ff proots
          (* helper @@ H.find tbl root; Format.fprintf ff "@;"; *)
        | None -> ()
        (* let rec helper ?(break=false) ({event; children} as node) =
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
        | None -> () *)
    end
  end
