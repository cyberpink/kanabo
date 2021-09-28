open Trep
    
module type implicits = sig
  type 'a dict
  exception Not_resolved
  val find : 'a trep -> 'a dict
  val register : 'a trep -> 'a dict -> unit
  type rule_entry = { pat: 'a. 'a trep -> 'a dict option }
  val register_rule : rule_entry -> unit
end

module MakeResolve(S:sig type 'a dict end) : 
    implicits with type 'a dict = 'a S.dict = struct
  include S
  exception Not_resolved

  type rule_entry = { pat: 'a. 'a trep -> 'a dict option }
  type reg_entry = rule_entry
  let registry : reg_entry list ref = ref []

  let register_rule : rule_entry -> unit = fun entry ->
    registry := entry :: !registry

  let register : type a. a trep -> a dict -> unit = fun trep dict ->
    let pat : type a. a trep -> a dict option = fun trep' ->
      match type_eq trep trep' with
      | Some Refl -> Some dict
      | _         -> None 
    in
    register_rule {pat}
      
  let find : type a. a trep -> a dict = fun trep ->
    let rec loop : reg_entry list -> a dict = function 
      | []   -> raise Not_resolved
      | {pat}::t ->
          match pat trep with
          | None      -> loop t
          | Some dict -> dict
    in loop !registry
end
