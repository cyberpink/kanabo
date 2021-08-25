(* Array Mapped Tries *)
module type S =
sig
  type 'a t
  type key

  val empty : 'a t
  val is_empty : 'a t -> bool
  val to_opt : 'a t -> 'a t option
  val from_opt : 'a t option -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val iter_values : ('a -> unit) -> 'a t -> unit
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a -> 'b option) ->
    (key -> 'c -> 'b option) ->
    (key -> 'a -> 'c -> 'b option) -> 'a t -> 'c t -> 'b t
  val symettric_difference : 'a t -> 'a t -> 'a t
  val intersection : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val min_binding_opt : 'a t -> (key * 'a) option
end

module AMT : S = struct
  module Key = struct
    type t = int
    let empty = (0, 0)
    let push (idx, key) frag = (idx + 5, (frag lsl idx) lor key)
    let view (_, key) = key
    type result = Unit of int | Pair of (int * int)
    let split key =
      match (key land 0b11111, key lsr 5) with
      | (idx, 0) -> Unit idx
      | (idx, tail) -> Pair (idx, tail)
  end
  type key = Key.t
  type 'a t = { branches : 'a t SVec.t; leaves : 'a SVec.t }
  let empty = { branches = SVec.empty; leaves = SVec.empty }
  let is_empty n = SVec.(is_empty n.leaves && is_empty n.branches)
  let to_opt n = if is_empty n then None else Some n
  let from_opt = function None -> empty | Some n -> n

  let rec find_opt key node =
    match Key.split key with
    | Unit idx -> SVec.find_opt node.leaves idx
    | Pair (idx, key') -> Option.bind (SVec.find_opt node.branches idx) (find_opt key')

  let rec update key fn node =
    match Key.split key with
    | Unit idx -> { node with leaves = SVec.update node.leaves idx fn }
    | Pair (idx, key') ->
      let branches' =
        SVec.update node.branches idx @@
        fun c -> Some (update key' fn @@ from_opt c)
      in { node with branches = branches' }

  let rec map fn node =
    { leaves = SVec.map fn node.leaves;
      branches = SVec.map (map fn) node.branches }
    
  let filter_map fn node =
    let rec aux key node =
      let leaves = SVec.filter_mapi (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves in
      let branches = SVec.filter_mapi (fun bkey -> aux @@ Key.push key bkey) node.branches in
      to_opt { leaves; branches }
    in from_opt @@ aux Key.empty node

  let rec iter_values fn node =
    SVec.iter fn node.leaves;
    SVec.iter (iter_values fn) node.branches

  let iter fn node =
    let rec aux key node =
      SVec.iteri (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves;
      SVec.iteri (fun bkey -> aux @@ Key.push key bkey) node.branches
    in aux Key.empty node

  let fold fn node init =
    let rec aux key node acc =
      SVec.fold (fun bkey -> aux @@ Key.push key bkey) node.branches @@
      SVec.fold (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves acc
    in aux Key.empty node init

  let mapi fn = filter_map @@ fun i x -> Some (fn i x)
  let add key value node = update key (fun _ -> Some value) node
  let remove key node = update key (fun _ -> None) node

  let merge map_a map_b merge_ab a b =
    let map_a' _key x = Some (filter_map map_a x) in
    let map_b' _key x = Some (filter_map map_b x) in
    let rec merge_ab' _key a b =
      to_opt @@
      { leaves = SVec.merge map_a map_b merge_ab a.leaves b.leaves;
        branches = SVec.merge map_a' map_b' merge_ab' a.branches b.branches }
    in from_opt @@ merge_ab' 0 a b

  include Mergeable.MergeSet(struct
      type key = Key.t
      type 'a s = 'a t
      let merge = merge
    end)

  let min_binding_opt m =
    let rec aux key m =
      match SVec.min m.leaves with
      | Some (k, x) -> Some (Key.view @@ Key.push key k, x)
      | None ->
        Option.bind (SVec.min m.branches) @@
        fun (k, b) -> aux (Key.push key k) b
    in aux Key.empty m
end
