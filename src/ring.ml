module type S = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
end

module Sum(R : S) : Monoid.S = struct
  type t = R.t
  let empty = R.zero
  let join = R.add
end

module Product(R : S) : Monoid.S = struct
  type t = R.t
  let empty = R.one
  let join = R.mul
end
