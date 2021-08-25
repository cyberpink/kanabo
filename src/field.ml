module type S = sig
  include Ring.S
  val recip : t -> t
  val div : t -> t -> t
end
