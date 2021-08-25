let factorial (type a) (module N : Ring.S with type t = a) =
  let rec factorial' = function
  | n when n = N.zero -> N.one
  | n when n = N.one -> N.one
  | n when n > N.one -> N.(mul n (factorial' (sub n N.one)))
  | _ -> raise @@ Invalid_argument "negative argument to factorial"
  in factorial'

module type GCDable = sig
  type t
  val zero : t
  val mul : t -> t -> t
  val div : t -> t -> t
  val abs : t -> t
  val rem : t -> t -> t
end

let gcd (type a) (module N : GCDable with type t = a) a b =
  let rec gcd' a b = N.(if b = zero then a else gcd' b (rem a b))
  in gcd' a b
    
let lcm (type a) (module N : GCDable with type t = a) a b =
  N.(div (abs (mul a b)) (gcd (module N) a b))

let log base n =
  let rec log' e = function
    | n when n <= 1 -> e (* lossy but works for ints for now *)
    | n -> log' (n / base) (e + 1)
  in log' 0 n
