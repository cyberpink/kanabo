type _ trep = ..
type (_, _) eq = Refl : ('a, 'a) eq

type _ trep += 
  | Unit   : unit trep
  | Int    : int trep
  | Bool   : bool trep
  | String : string trep
  | Option : 'a trep -> 'a option trep
  | List   : 'a trep -> 'a list trep
  | Pair   : 'a trep * 'b trep -> ('a * 'b) trep
  | Fun    : 'a trep * 'b trep -> ('a -> 'b) trep

let type_eq : type a b. a trep -> b trep -> (a, b) eq option = fun x y ->
  if Obj.repr x = Obj.repr y then Some (Obj.magic Refl) else None
