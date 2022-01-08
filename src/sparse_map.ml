module type SparseMapBase = sig
  type 'a t
  type key
  val is_empty : 'a t -> bool
  val empty : 'a t
  val find_opt : key -> 'a t -> 'a option
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val intersection : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

module type SparseElem = sig
  type t
  val to_opt : t -> t option
  val of_opt : t option -> t
end

module Make(M : SparseMapBase)(E : SparseElem) = struct
  type t = E.t M.t      
  type key = M.key
  type elem = E.t
           
  let empty = M.empty
  let is_empty = M.is_empty
  let add k v a = match E.to_opt v with None -> a | Some v -> M.add k v a
  let singleton k v = add k v empty
  let find k a = E.of_opt @@ M.find_opt k a
  let update k f a = M.update k (fun x -> E.(to_opt (f (of_opt x)))) a
  let map f a = M.filter_map (fun _ e -> E.to_opt (f e)) a      
  let union f a b = M.union (fun k av bv -> E.to_opt (f k av bv)) a b
  let intersection f a b = M.intersection (fun k av bv -> E.to_opt (f k av bv)) a b
  let map2 f a b = M.intersection (fun _ av bv -> E.to_opt (f av bv)) a b
  let fold = M.fold
  let iter = M.iter
  let to_opt m = if M.is_empty m then None else Some m
  let of_opt = function None -> empty | Some m -> m
end
