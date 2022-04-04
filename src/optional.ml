module type S = sig
  type t
  val to_opt : t -> t option
  val of_opt : t option -> t
end

type 'a s = (module S with type t = 'a)
