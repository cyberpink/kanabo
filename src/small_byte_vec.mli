type t
type key = int
type elt = int
val max_len : int
val init : int -> (key -> elt) -> t
val empty : t
val is_empty : t -> bool
val set : t -> key -> elt -> t
val get : t -> key -> elt
