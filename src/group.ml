module type S = sig
  include Monoid.S
  val inv : t -> t
end

let pow : type a. (module S with type t = a) -> a -> int -> a =
  fun (module G) b e ->
  if e < 0 then
    Monoid.pow (module G) (G.inv b) (Int.neg e)
  else 
    Monoid.pow (module G) b e


module FromField(N : Field.S) = struct
  type t = N.t
  let empty = N.one
  let join = N.mul
  let inv = N.recip
end
