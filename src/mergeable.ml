module type S = sig
  type key
  type 'a s
  val merge :
    (key -> 'a -> 'c option) ->
    (key -> 'b -> 'c option) ->
    (key -> 'a -> 'b -> 'c option) ->
    'a s -> 'b s -> 'c s
end

module MergeSet (S : S) : sig
  val symettric_difference : 'a S.s -> 'a S.s -> 'a S.s
  val intersection : (S.key -> 'a -> 'b -> 'c option) -> 'a S.s -> 'b S.s -> 'c S.s
  val union : (S.key -> 'a -> 'a -> 'a option) -> 'a S.s -> 'a S.s -> 'a S.s
end = struct
  let keep _key x = Some x
  let drop _key _ = None
  let drop2 _key _ _ = None

  let symettric_difference a b = S.merge keep keep drop2 a b
  let intersection fn a b = S.merge drop drop fn a b
  let union fn a b = S.merge keep keep fn a b
end
