(* Key and value polymorphic wrapper around ocaml's Stdlib.Map.
   inj/prj should be relatively safe, being behind a generative functor?
*)
type ('k, 'v) t

type ('k, 'v) s = ('k, 'v) t
module type S = sig
  include Map.S
  val prj : (key, 'v) s -> 'v t
  val inj : 'v t -> (key, 'v) s
end

type 'k impl = (module S with type key = 'k)
module Make(O : Map.OrderedType) () : S with type key = O.t = struct
  module M = Map.Make(O)
  external prj : (M.key, 'v) t -> 'v M.t = "%identity"
  external inj : 'v M.t -> (M.key, 'v) t = "%identity"
  include M
end
