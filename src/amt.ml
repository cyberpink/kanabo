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
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val iter_values : ('a -> unit) -> 'a t -> unit
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge' :
    (key -> 'a -> 'b option) ->
    (key -> 'c -> 'b option) ->
    (key -> 'a -> 'c -> 'b option) -> 'a t -> 'c t -> 'b t
  val symettric_difference : 'a t -> 'a t -> 'a t
  val intersection : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val min_binding_opt : 'a t -> (key * 'a) option
  val pp_print :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?pp_link:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(* branch and leaf vectors can be totally separate
   can use different key modules as well

   example would be using amt as a bitmap
   the leave nodes should just be a bitmap rather than a bitmapped vector
*)
module AMT : (S with type key = int) = struct
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
    | Unit idx -> SVec.find_opt idx node.leaves
    | Pair (idx, key') -> Option.bind (SVec.find_opt idx node.branches) (find_opt key')

  let rec update key fn node =
    match Key.split key with
    | Unit idx -> { node with leaves = SVec.update idx fn node.leaves }
    | Pair (idx, key') ->
      let branches' =
        node.branches |> SVec.update idx @@
        fun c -> Some (update key' fn @@ from_opt c)
      in { node with branches = branches' }

  let rec map fn node =
    { leaves = SVec.map fn node.leaves;
      branches = SVec.map (map fn) node.branches }

  let filter fn node =
    let rec aux node =
      let leaves = SVec.filter fn node.leaves in
      let branches = SVec.filter_map (fun _ -> aux) node.branches in
      to_opt { leaves; branches }
    in from_opt @@ aux node

  let filter_map fn node =
    let rec aux key node =
      let leaves = SVec.filter_map (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves in
      let branches = SVec.filter_map (fun bkey -> aux @@ Key.push key bkey) node.branches in
      to_opt { leaves; branches }
    in from_opt @@ aux Key.empty node

  let rec iter_values fn node =
    SVec.iter_values fn node.leaves;
    SVec.iter_values (iter_values fn) node.branches

  let iter fn node =
    let rec aux key node =
      SVec.iter (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves;
      SVec.iter (fun bkey -> aux @@ Key.push key bkey) node.branches
    in aux Key.empty node

  let fold fn node init =
    let rec aux key node acc =
      SVec.fold (fun bkey -> aux @@ Key.push key bkey) node.branches @@
      SVec.fold (fun lkey -> fn @@ Key.view @@ Key.push key lkey) node.leaves acc
    in aux Key.empty node init

  let mapi fn = filter_map @@ fun i x -> Some (fn i x)
  let add key value node = update key (fun _ -> Some value) node
  let remove key node = update key (fun _ -> None) node

  let merge' map_a map_b merge_ab a b =
    let map_a' _key x = Some (filter_map map_a x) in
    let map_b' _key x = Some (filter_map map_b x) in
    let rec merge_ab' _key a b =
      to_opt @@
      { leaves = SVec.merge' map_a map_b merge_ab a.leaves b.leaves;
        branches = SVec.merge' map_a' map_b' merge_ab' a.branches b.branches }
    in from_opt @@ merge_ab' 0 a b

  include Mergeable.MergeSet(struct
      type key = Key.t
      type nonrec 'a t = 'a t
      let merge' = merge'
    end)

  let min_binding_opt m =
    let rec aux key m =
      match SVec.min_opt m.leaves with
      | Some (k, x) -> Some (Key.view @@ Key.push key k, x)
      | None ->
        Option.bind (SVec.min_opt m.branches) @@
        fun (k, b) -> aux (Key.push key k) b
    in aux Key.empty m

  let pp_print =
    let open Format in
    let default_sep = fun ppf () -> fprintf ppf ";@ " in
    let default_link = fun ppf () -> fprintf ppf "@ =@ " in
    fun ?(pp_sep = default_sep) ?(pp_link = default_link) pp_key pp_val ppf ->
      fprintf ppf "{@ %a@ }" @@ fun ppf ->
      iter @@ fun k v ->
      pp_key ppf k;
      pp_link ppf ();
      pp_val ppf v;
      pp_sep ppf ()
end
