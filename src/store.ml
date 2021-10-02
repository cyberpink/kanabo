module type storage = sig
  type 'a t
  val find_opt : int -> 'a t -> 'a option 
  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val update : int -> ('a option -> 'a option) -> 'a t -> 'a t
end

module Make(Storage : storage) () = struct
  type 'a t = { counter : int; storage : 'a Storage.t }
  type key = Key of int

  let empty = { counter = 0; storage = Storage.empty }
              
  let alloc = fun ?value store ->
    let c = store.counter in
    let counter = c + 1 in
    let storage =
      match value with
      | None -> store.storage
      | Some v -> Storage.add c v store.storage
    in
    (Key c, { counter; storage })

  let get (Key key) store =
    match Storage.find_opt key store.storage with
    | None -> None
    | Some v -> Some v
                  
  let get_exn key store =
    match get key store with
    | None -> raise Not_found
    | Some x -> x

  let set (Key key) value store =
    { store with storage = Storage.add key value store.storage }

  let update (Key key) fn store =
    { store with storage = Storage.update key fn store.storage }
    
end
