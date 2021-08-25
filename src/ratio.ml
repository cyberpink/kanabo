module type Divisible = sig
  type t
  include Ring.S with type t := t
  include Math.GCDable with type t := t
  val of_int : int -> t
end

type 'a ratio = { n : 'a; d : 'a }

let to_string (type a) (module N : Coercible.ToString with type t = a) n =
  N.(to_string n.n ^ " / " ^ to_string n.d)

let to_float (type a) (module N : Coercible.ToFloat with type t = a) n =
  Float.(div (N.to_float n.n) (N.to_float n.d))

let map f {n; d} = { n = f n; d = f d }

module Make(N : Divisible) = struct
  type t = N.t ratio
  let make n d =
    let open N in
    let gcd = Math.gcd (module N) n d in
    { n = div n gcd; d = div d gcd }

  let of_int i = make (N.of_int i) N.one
      
  let zero = make N.zero N.one
  let one = make N.one N.one

  let norm2 a b = N.(mul a.d b.d, mul a.n b.d, mul b.n a.d)

  let neg a = { a with n = N.neg a.n }
  
  let add a b =
    let d', an', bn' = norm2 a b in
    make N.(add an' bn') d'

  let sub a b =
    let d', an', bn' = norm2 a b in
    make N.(sub an' bn') d'

  let inv a = make a.d a.n
  let recip = inv
  let mul a b = N.(make (mul a.n b.n) (mul a.d b.d))
  let div a b = mul a (inv b)
  
  let compare a b =
    let (_, an', bn') = norm2 a b in
    compare an' bn'
end
