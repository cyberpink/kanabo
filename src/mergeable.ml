module type S = sig
  type key
  type 'a t
  val merge :
    (key -> 'a -> 'c option) ->
    (key -> 'b -> 'c option) ->
    (key -> 'a -> 'b -> 'c option) ->
    'a t -> 'b t -> 'c t
end

let keep _key x = Some x
let drop _key _ = None
let drop2 _key _ _ = None

module MergeSet (S : S) = struct
  let symettric_difference a b = S.merge keep keep drop2 a b
  let intersection fn a b = S.merge drop drop fn a b
  let union fn a b = S.merge keep keep fn a b
end


module type SHK = sig
  type key
  type 'v t
  include module type of HK.Make(struct type nonrec 'v t = 'v t end)

  val merge :
    (key -> 'a -> 'c option) ->
    (key -> 'b -> 'c option) ->
    (key -> 'a -> 'b -> 'c option) ->
    ('a -> hk) HK.t -> ('b -> hk) HK.t -> ('c -> hk) HK.t
end

let symettric_difference (type a) (module S : SHK with type hk = a) a b =
 S.merge keep keep drop2 a b

let intersection (type a k) (module S : SHK with type hk = a and type key = k) fn a b =
  S.merge drop drop fn a b

let union (type a k) (module S : SHK with type hk = a and type key = k) fn a b =
  S.merge keep keep fn a b
