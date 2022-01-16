module type S = sig
  type t
  val to_opt : t -> t option
  val of_opt : t option -> t
end
