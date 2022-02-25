module type Elt = sig
  type t
  type fragment
  type buffer

  val fold_left : ('a -> fragment -> 'a) -> 'a -> t -> 'a
  val reify : buffer -> t
  val empty : buffer
  val append : buffer -> fragment -> buffer
end

(* parameterize map type *)
module Make(Elt : Elt)(E : Stdlib.Map.OrderedType with type t = Elt.fragment) = struct
  module M = Map.Make(E)
  type t = { size : int; mem : bool; branches : t M.t }
  type elt = Elt.t

  let empty = { size = 0; mem = false; branches = M.empty }
  let sentinel = { size = 1; mem = true; branches = M.empty }

  let size b = b.size
  let is_empty d = M.is_empty d.branches && not d.mem
  let of_opt = function None -> empty | Some d -> d
  let to_opt d = if is_empty d then None else Some d

  let make mem branches =
    let s = if mem then 1 else 0 in
    { size = M.fold (fun _ cs s -> cs.size + s) branches s;
      mem; branches;
    }

  let singleton s =
    Elt.fold_left
      (fun m c t -> m (make false (M.singleton c t)))
      (fun x -> x)
      s
      sentinel

  let choose_opt d =
    let rec loop last buf b =
      let last' = if b.mem then Some buf else last in
      match M.choose_opt b.branches with
      | None -> Option.map Elt.reify last'
      | Some (f, b') ->
        loop last' (Elt.append buf f) b'
    in loop None Elt.empty d
    
  (* This would be better done using a zipper.
     As would building output via CPS. *)
  let rec filter_incr p b =
    let terminate = function
      | `Done b -> b
      | _ -> failwith "invalid filter termination"
    in
    let mem = if b.mem then terminate (p None) else false in
    let branches =
      M.filter_map
        (fun c children ->
           match p (Some c) with
           | `Done false -> None
           | `Done true -> Some children
           | `Ok p' -> to_opt @@ filter_incr p' children)
        b.branches
    in make mem branches

  let rec union a b =
    make (a.mem || b.mem) @@
    M.union (fun _ a b -> Some (union a b)) a.branches b.branches

  let add s d = union d @@ singleton s

  let mem s d =
    try (Elt.fold_left (fun b c -> M.find c b.branches) d s).mem
    with Not_found -> false

  let fold f d m =
    let rec loop m pos b buf =
      let m = if b.mem then f (Elt.reify buf) m else m in
      M.fold
        (fun c b m -> loop m (pos + 1) b (Elt.append buf c))
        b.branches m
    in loop m 0 d Elt.empty

  let iter f d =
    let rec loop pos b buf =
      if b.mem then f (Elt.reify buf);
      M.iter
        (fun c b -> loop (pos + 1) b (Elt.append buf c))
        b.branches
    in loop 0 d Elt.empty

  let filter p d = fold (fun w m -> if p w then add w m else m) d empty
  let to_seq d = fold (fun w m -> fun () -> Seq.Cons (w, m)) d Seq.empty
  let of_seq s = Seq.fold_left union empty @@ Seq.map singleton s      
end
