include Stdlib.Int
type scalar = t
let scale = mul

module Sum = struct
  let empty = 0
  let join = add
  let to_opt i = match i with 0 -> None | i -> Some i
  let of_opt i = match i with Some i -> i | None -> 0
end
