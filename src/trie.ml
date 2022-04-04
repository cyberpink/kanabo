module type Elt = sig
  type t
  type fragment
  val to_seq : t -> fragment Seq.t
  
  type buffer
  val reify : buffer -> t
  val empty : buffer
  val append : buffer -> fragment -> buffer
end

module type Map = sig
  type 'a t
  type key
  val empty : 'a t
  val size : 'a t -> int
  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val min_opt : 'a t -> (key * 'a) option
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val intersection : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val fold : (key-> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val remove : key -> 'a t -> 'a t
end

module Make(M : Map)(Elt : Elt with type fragment = M.key) = struct
  type 'a t = { value : 'a option; branches : 'a t M.t }
  type elt = Elt.t
  let make value branches = { value; branches; }
  let empty = { value = None; branches = M.empty }
  let rec size b =
    M.fold (fun _ bs s -> (size bs) + s) b.branches
      (match b.value with Some _ ->  1 | None -> 0)
  let is_empty b = M.is_empty b.branches && Option.is_none b.value
  let of_opt = function None -> empty | Some d -> d
  let to_opt d = if is_empty d then None else Some d

  let rec compare cmp a b =
    let vc = Option.compare cmp a.value b.value in
    if vc <> 0
    then vc
    else
      M.compare (compare cmp) a.branches b.branches

  let add k v b =
    let rec aux key b =      
      match Seq.uncons key with
      | None -> { b with value = Some v }
      | Some (f, key') ->
        { b with
          branches =
            M.update f (fun b -> b |> of_opt |> aux key' |> to_opt) b.branches }
    in aux (Elt.to_seq k) b

  let singleton k v = add k v empty

  let min_binding_opt d =
    let rec loop buf b =
      match b.value with
      | Some v -> Some (Elt.reify buf, v)
      | None ->
        match M.min_opt b.branches with
        | None -> None
        | Some (f, b') -> loop (Elt.append buf f) b'
    in loop Elt.empty d

  (* TODO: version that respects Map.union api *)
  let rec union fn a b =
    let value = 
      match (a.value, b.value) with
      | None, None -> None
      | Some x , None | None, Some x -> Some x
      | Some a, Some b -> fn a b
    in
    { value;
      branches = M.union (fun _ a b -> to_opt (union fn a b)) a.branches b.branches }

  let rec intersection fn a b =
    let value = 
      match (a.value, b.value) with
      | _, None | None, _ -> None
      | Some a, Some b -> fn a b
    in
    { value;
      branches =
        M.intersection (fun _ a b -> to_opt (intersection fn a b)) a.branches b.branches
    }

  let rec subtract fn a b =
    let value = 
      match (a.value, b.value) with
      | None, _ -> None
      | Some a, None -> Some a
      | Some a, Some b -> fn a b
    in
    { value;
      branches =
        M.merge (fun _ a b -> to_opt (subtract fn (of_opt a) (of_opt b))) a.branches b.branches
    }

  let mem s d =
    try
      let rec loop b key =
        match Seq.uncons key with
        | None -> (match b.value with Some _ -> true | None -> false)
        | Some (f, key') -> loop (M.find f b.branches) key'
      in loop d (Elt.to_seq s)
    with Not_found -> false

  let filter_incr p_v p_b state b =
    let rec aux b state =
      let value = Option.bind b.value (fun v -> p_v v state) in
      let branches =
        M.filter_map
          (fun i b -> Option.bind (p_b i state) (fun s' -> to_opt (aux b s')))
          b.branches
      in
      { value; branches }
    in aux b state
      
  let iter_incr p_v p_b state b =
    let rec aux b state =
      Option.iter (fun v -> p_v v state) b.value;
      M.iter
        (fun i b -> Option.iter (aux b) (p_b i state))
        b.branches
    in aux b state

  let iter_uncons fn b =
    M.iter
      (fun i b -> fn i b.value b)
      b.branches


  let fold f d m =
    let rec loop buf b m =
      M.fold (fun c -> loop (Elt.append buf c)) b.branches @@
      match b.value with Some v -> f (Elt.reify buf) v m | None -> m
    in loop Elt.empty d m

  let iter f b =
    let rec loop buf b =
      Option.iter (f (Elt.reify buf)) b.value;
      M.iter (fun c -> loop (Elt.append buf c)) b.branches
    in loop Elt.empty b
      
  let map fn b =
    let rec loop b =
      let value = Option.map fn b.value in
      let branches = M.map (fun b -> loop b) b.branches in
      { value; branches }
    in loop b

  let filter p b =
    let rec loop buf b =
      let value =
        match b.value with
        | Some v -> if (p (Elt.reify buf) v) then b.value else None
        | None -> None in
      let branches =
        M.filter_map
          (fun c b -> to_opt @@ loop (Elt.append buf c) b)
          b.branches
      in { value; branches }
    in loop Elt.empty b

  let to_seq d = fold (fun w v m -> fun () -> Seq.Cons ((w, v), m)) d Seq.empty
  let of_seq s = Seq.fold_left (fun t (k, v) -> add k v t) empty s
end
