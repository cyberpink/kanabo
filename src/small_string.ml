module V = Small_byte_vec
type t = V.t
let of_string s =    
  match String.length s with
  | len when len > V.max_len ->
    raise (Invalid_argument "String to long to create small string")
  | len -> V.init len @@ fun i -> Char.code (String.get s i)

let get s i = Char.chr @@ V.get s i
let set s i c = V.set s i (Char.code c)
