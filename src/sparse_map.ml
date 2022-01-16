module type HKMap = sig
  type ('k, 'v) t

  include module type of HK.Make2(struct type nonrec ('k, 'v) t = ('k, 'v) t end)

  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find_opt : 'k -> ('k, 'v) t -> 'v option
  val update : 'k -> ('v option -> 'v option) -> ('k, 'v) t -> ('k, 'v) t
end

let find (type m v) (module M : HKMap with type hk = m)
    (module E : Optional.S with type t = v) k a =
  E.of_opt @@ M.find_opt k (M.prj a)

let add (type m v) (module M : HKMap with type hk = m)
    (module E : Optional.S with type t = v) k v a =
  match E.to_opt v with None -> a | Some v -> M.inj (M.add k v (M.prj a))

let update (type m v) (module M : HKMap with type hk = m)
    (module E : Optional.S with type t = v) k f a =
  M.inj @@ M.update k (fun x -> E.(to_opt (f (of_opt x)))) (M.prj a)

module type Mappable = sig
  include HKMap
  val filter_map : ('k -> 'a -> 'b option) -> ('k, 'a) t -> ('k, 'b) t
end

let map
   (type m v)
    (module M : Mappable with type hk = m)
    (module E : Optional.S with type t = v) f a =
  M.(inj @@ filter_map (fun _ e -> E.to_opt (f e)) (prj a))

module type Mergeable = sig
  include HKMap
  val union : ('k -> 'a -> 'a -> 'a option) -> ('k, 'a) t -> ('k, 'a) t -> ('k, 'a) t
  val intersection :
    ('k -> 'a -> 'b -> 'c option) -> ('k, 'a) t -> ('k, 'b) t -> ('k, 'c) t
end

let union
   (type m v)
    (module M : Mergeable with type hk = m)
    (module E : Optional.S with type t = v)  f a b =
  M.(inj @@ union (fun k av bv -> E.to_opt (f k av bv)) (prj a) (prj b))

let intersection
   (type m v)
    (module M : Mergeable with type hk = m)
    (module E : Optional.S with type t = v) f a b =
  M.(inj @@ intersection (fun k av bv -> E.to_opt (f k av bv)) (prj a) (prj b))

let map2 = intersection
