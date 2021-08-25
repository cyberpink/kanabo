module type ToFloat = sig
  type t
  val to_float : t -> float
end

module type ToString = sig
  type t
  val to_string : t -> string
end

module type FromInt = sig
  type t
  val from_int : int -> t
end


