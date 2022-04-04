(* Key and value polymorphic wrapper around ocaml's Stdlib.Map.
   inj/prj should be relatively safe, being behind a generative functor?
*)
type ('k, 'v) t

type ('k, 'v) _t = ('k, 'v) t
module type S = sig
  include Stdlib.Map.S
  val prj : (key, 'v) _t -> 'v t
  val inj : 'v t -> (key, 'v) _t
end

type 'k impl = (module S with type key = 'k)
module Make(O : Map.OrderedType) () : S with type key = O.t = struct
  module M = Stdlib.Map.Make(O)
  external prj : (M.key, 'v) t -> 'v M.t = "%identity"
  external inj : 'v M.t -> (M.key, 'v) t = "%identity"
  include M
end

module Sparse = struct
  let find : type k v. k impl -> v Optional.s ->
    k -> (k, v) t -> v =
    fun (module M) (module O) k m -> O.of_opt M.(find_opt k (prj m))

  let add : type k v. k impl -> v Optional.s ->
    k -> v -> (k, v) t -> (k, v) t =
    fun (module M) (module O) k v m ->
    M.(inj @@ update k (fun _ -> O.to_opt v) (prj m))

  let update : type k v. k impl -> v Optional.s ->
    k -> (v -> v) -> (k, v) t -> (k, v) t =
    fun (module M) (module O) k f m ->
    M.(inj @@ update k O.(fun x -> to_opt (f (of_opt x))) (prj m))

  let merge : type k a b c.
    k impl -> a Optional.s -> b Optional.s -> c Optional.s ->
    (k -> a -> b -> c) -> (k, a) t -> (k, b) t -> (k, c) t =
    fun (module M) (module A) (module B) (module C) fn a b ->
    M.(inj @@ merge (fun k a b -> C.to_opt (fn k (A.of_opt a) (B.of_opt b)))
         (prj a) (prj b))
end
