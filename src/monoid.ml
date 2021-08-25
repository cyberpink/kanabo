module type S = sig
  type t
  val empty : t
  val join : t -> t -> t
end

module FromFn(F : sig type t end) = struct
  type t = F.t
  let empty = fun x -> x
  let join f g = fun a -> f (g a)
end

let pow : type a. (module S with type t = a) -> a -> int -> a =
  fun (module G) b e ->
  let rec pow' n m = function
    | 1 -> G.join n m
    | e when e mod 2 = 0 -> pow' (G.join n n) m (e / 2)
    | e -> pow' n (G.join n m) (e - 1)
  in
  if e < 0 then
    raise (Invalid_argument "negative exponent")
  else if e = 0 then
    G.empty
  else 
    pow' b G.empty e
