open MiniKanrenCore

module TreeLogger =
  struct
    type node = { event : Listener.event; children : StateId.t list }

    let make_node e = { event = e; children = []; }

    module H = Hashtbl.Make(StateId)

    let default_filter = Listener.(fun (id, event) ->
      match event with
      | Custom str -> not (String.equal "root" str)
      | Success | Failure _ -> false
      | _ -> true
    )

    type color = Green | Yellow | Red | White

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
      | Cont _      ->
        if (cs = []) then Yellow
        else if List.for_all ((==) Red) cs then Red
        else if List.exists ((==) Green) cs then Green
        else if List.exists ((==) Yellow) cs then Yellow
        else White
      | Unif _      -> let [c] = cs in c
      | Diseq _     -> let [c] = cs in c
      | Goal (_, _) -> let [c] = cs in c
      | Answer _    -> Green
      | _           -> White
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

    (** box - structure that contains information for pretty-printing a single node in search tree.*)
    type box =
      (* id of the node in the search tree *)
      { id: StateId.t
      (* string that describes the node *)
      ; str: string
      (* color of node *)
      ; c: color
      (* list of children nodes *)
      ; inner: box list
      (* flag that specifies whether the id of node should be printed along with text *)
      ; with_id: bool
      (* optional link to another location in the search tree *)
      ; link : StateId.t option
      (* for goal-nodes - a list of all answers that call to this goal has produced *)
      ; answers: (string * StateId.t) list
      }

    let make_box ?(inner=[]) ?(with_id=true) ?(link=None) ?(answers=[]) id str c =
      { id; str; c; inner; with_id; link; answers }

    (** pinfo - contains information that propogates bottom-up
        through the search tree in order to construct corresponded tree of boxes. *)
    type pinfo = { c: color; boxes: box list; answers: (string * StateId.t) list }

    let is_leaf {inner} = (inner == [])

    let is_foldable boxes = List.for_all is_leaf boxes

    (** auxiliary function that folds sequantial continuation boxes *)
    let rec fold_boxes = function
    | []  -> []
    | [b] -> [b]
    | ({id=id1} as b1)::{id=id2; inner}::bs when (id1=id2) && (List.length inner = 1) ->
      let [b2] = inner in
      b1::(fold_boxes @@ b2::bs)
    | bs -> bs

    let list_max x::xs = List.fold_left (fun acc x -> max acc x) x xs

    let make_pinfo (id, event) c boxes answers = Listener.(
      match event with
      | Conj when boxes = [] ->
        { c=c; boxes=[]; answers }
      | Disj when boxes = [] ->
        { c=c; boxes=[]; answers }
      | Conj ->
        let boxes = fold_boxes boxes in
        if List.length boxes = 1 then
          { c; boxes; answers }
        else if List.for_all is_leaf boxes then
          let ids = List.map (fun {id} -> id) boxes in
          let id  = list_max ids in
          let str = "(" ^ (String.concat ") &&& (" @@ List.map (fun {str} -> str) boxes) ^ ")" in
          let box = make_box id str c in
          { c; boxes=[box]; answers }
        else
          let str = "&&&" in
          let box = make_box id str c ~with_id:false ~inner:boxes in
          { c; boxes=[box]; answers }
      | Disj ->
        (* if List.for_all is_leaf boxes then
          let str = "(" ^ (String.concat ") ||| (" @@ List.map (fun {str} -> str) boxes) ^ ")" in
          let box = { id; str; c; inner=[] } in
          { c; boxes=[box] }
        else *)
        if List.length boxes = 1 then
          { c; boxes; answers }
        else
          let str = "conde" in
          let box = make_box id str c ~inner:boxes ~with_id:false in
          { c; boxes=[box]; answers }
      | Cont id ->
        if boxes<>[] then
          let box = make_box id "continue" c ~inner:boxes ~with_id:false ~link:(Some id) in
          { c; boxes=[box]; answers }
        else
          { c; boxes=[]; answers }
      | Unif _  ->
        let str = string_of_event event in
        let box = make_box id str c in
        { c; boxes=[box]; answers=[] }
      | Diseq _  ->
        let str = string_of_event event in
        let box = make_box id str c in
        { c; boxes=[box]; answers=[] }
      | Answer (_, answ) ->
        let answer = (String.concat " " answ, id) in
        { c; boxes=[]; answers=[answer] }
      | Goal (_, _) ->
        let str = string_of_event event in
        let box = make_box id str c ~inner:boxes ~answers in
        { c; boxes=[box]; answers=[] }
      | _ ->
        let str = string_of_event event in
        let box = make_box id str c ~inner:boxes in
        { c; boxes=[box]; answers }
    )

    (** constructs a node of pretty-print tree of boxes from a node of a search tree
        and list of children box-nodes (i.e. in a bottom-up manner) *)
    let to_ptree : (StateId.t * Listener.event -> bool) -> StateId.t * Listener.event -> pinfo list -> pinfo =
        fun filter ((id, event) as node) children ->
        let boxes = List.concat @@ List.map (fun {boxes} -> boxes) children in
        let answers = List.concat @@ List.map (fun {answers} -> answers) children in
        let colors = List.map (fun {c} -> c) children in
        let c = color_of_event event colors in
        if (filter node) then
          make_pinfo node c boxes answers
        else
          { c; boxes; answers }

    (** relabels search tree node's ids to the consecutive sequence of int labels *)
    let make_labels : box list -> int H.t = fun boxes ->
      let module S = Set.Make(StateId) in
      let ordered = ref S.empty in
      let add_id id = (ordered := S.add id !ordered) in
      let rec helper {id; inner; with_id; link; answers} =
        if with_id then (add_id id);
        if link <> None then (let Some id = link in add_id id);
        List.iter (fun (_, id) -> add_id id) answers;
        List.iter helper inner
      in
      List.iter helper boxes;
      let i = ref 0 in
      let tbl = H.create 31 in
      S.iter (fun id -> incr i; H.add tbl id !i) !ordered;
      tbl

    let print_ptree ff labels roots =
      let rec helper {id; c; str; inner; with_id; link; answers } =
        let str_of_id id = Printf.sprintf "{%d} " (H.find labels id) in
        let str_id = if with_id then str_of_id id else "" in
        let str_link = if link<>None then (let Some id = link in str_of_id id) else "" in
        let cstr = color_str c @@ Printf.sprintf "%s%s %s" str_id str str_link in
        Format.fprintf ff "@[<v 2>%s" @@ cstr;
        if answers <> [] then
          print_answers c answers
        else
          ();
        print_inner ~break:(true) inner;
        Format.fprintf ff "@]"
      and print_inner ?(break=false) = function
        | []    -> ()
        | [x]   ->
          if break then Format.fprintf ff "@;" else ();
          helper x
        | x::xs ->
          if break then Format.fprintf ff "@;" else ();
          helper x;
          List.iter (fun x -> Format.fprintf ff "@;"; helper x) xs;
      and print_answers c = function
        | [] -> ()
        | xs ->
          let print_answer (answ, id) =
            let label =
              try H.find labels id
              with Not_found -> 0
            in
            Format.fprintf ff "@;%s" @@ color_str c (Printf.sprintf "{%d} %s" label answ)
          in
          Format.fprintf ff "@;%s" @@ color_str c "Answers:";
          List.iter print_answer xs
      in
      print_inner roots;
      Format.fprintf ff "@;"

    class type t = object
      method init: StateId.t -> unit
      method on_event: Listener.event -> StateId.t -> StateId.t -> unit
      method fold : 'a. (StateId.t * Listener.event -> 'a list -> 'a) -> 'a
      method print: ?filter:(StateId.t * Listener.event -> bool) -> Format.formatter -> unit
    end

    let create () = object (self)
      val tbl : node H.t = H.create 31
      val root : StateId.t option ref = ref None

      method init id =
        H.reset tbl;
        root := Some id;
        H.add tbl id @@ make_node (Listener.Custom "root")

      method on_event e pid id =
        let node = H.add tbl id @@ make_node e in
        let parent = H.find tbl pid in
        H.replace tbl pid { parent with children = id :: parent.children }

      method fold : 'a. (StateId.t * Listener.event -> 'a list -> 'a) -> 'a = fun f ->
        let rec helper id =
          let {event; children} = H.find tbl id in
          f (id, event) @@ List.map helper (List.rev children)
        in
        match !root with
        | Some root -> helper root
        | None -> raise (Invalid_argument "Empty tree")

      method print ?(filter = default_filter) ff =
        match !root with
        | Some root ->
          let {boxes} = self#fold (to_ptree filter) in
          let labels = make_labels boxes in
          print_ptree ff labels boxes
        | None -> ()

    end
  end
