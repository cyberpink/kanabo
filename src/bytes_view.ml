module type S = sig
  type t = private Bytes.t
  val elt_size : int
  val of_bytes : Bytes.t -> t
  val to_bytes : t -> Bytes.t
  val set : t -> int -> int -> unit
  val get : t -> int -> int
end

module U8 : S = struct
  type t = Bytes.t
  let elt_size = 1
  let of_bytes b = b
  let to_bytes b = b
  let get bytes i =  Bytes.get_uint8 bytes (i * elt_size)
  let set bytes i v = Bytes.set_uint8 bytes (i * elt_size) v
end

module U16_be : S = struct
  type t = Bytes.t
  let elt_size = 2
  let of_bytes b = b
  let to_bytes b = b
  let get bytes i = Bytes.get_uint16_be bytes (i * elt_size)
  let set bytes i v = Bytes.set_uint16_be bytes (i * elt_size) v
end
