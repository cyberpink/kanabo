(* Sparse Vectors using bitmaps
   Maximum size of 31 or 63 depending on arch
   Not safe at all right now
*)

type key = int
type 'a t = { bitmap : int; storage : 'a array }
let empty = { bitmap = 0; storage = Array.empty }
let is_empty v = v.bitmap = 0
let length v = Array.length v.storage
let size = length
let mem v i = Bits.get v.bitmap i <> 0

let find i v =
  if mem v i
  then Array.unsafe_get v.storage (Bits.before v.bitmap i)
  else raise Not_found

let find_opt i v =
  if mem v i
  then Some (Array.unsafe_get v.storage (Bits.before v.bitmap i))
  else None

let singleton i x =
  { bitmap = Bits.set 0 i; storage = [| x |] }

let update i fn v =
  let pos = Bits.before v.bitmap i in
  if mem v i then
    match fn @@ Some (Array.unsafe_get v.storage pos) with
    | None ->
      (match Bits.flip v.bitmap i with
       | 0 -> empty
       | bm' -> { bitmap = bm'; storage = Array.remove v.storage pos })
    | Some x -> { v with storage = Array.copy_set v.storage pos x }
  else
    match fn None with
    | None -> v
    | Some x ->
      { bitmap = Bits.set v.bitmap i;
        storage = Array.splice_at v.storage pos x }

let remove i v = update i (fun _ -> None) v
let add i x v = update i (fun _ -> Some x) v
let map fn v = { v with storage = Array.map fn v.storage }
let iter_values fn v = Array.iter fn v.storage
let iter fn v = Bits.iteri (fun i idx -> fn idx @@ Array.unsafe_get v.storage i) v.bitmap
let for_all fn v =
  let exception False in
  try
    Bits.iteri
      (fun i idx -> if not (fn idx @@ Array.unsafe_get v.storage i) then raise False)
      v.bitmap;
    true
  with False -> false

let mapi fn v =
  let indexes = Bits.indexes v.bitmap in
  let storage = Array.mapi (fun i x -> fn (Bytes.get_int8 indexes i) x) v.storage in
  { v with storage }

let to_seq v = Seq.map (fun (idx, i) -> (idx, Array.unsafe_get v.storage i)) @@ Bits.to_seqi v.bitmap

let compare compare a b =
  let bc = Bits.compare a.bitmap b.bitmap in
  if bc <> 0
  then bc
  else
    let len = Bits.popcount a.bitmap in
    let rec loop i =
      if i = len then
        0
      else
        let av = Array.unsafe_get a.storage i in
        let bv = Array.unsafe_get b.storage i in
        let ec = compare av bv in
        if ec <> 0
        then ec
        else
          loop (succ i)
    in loop 0

(* faster than using an array buffer that gets trimmed, somehow *)
let empty_buff () = ref (0, [])
let reify_buff b =
  let (bm, buff) = !b in
  if bm = 0 then empty else { bitmap = bm; storage = Array.of_list buff }
let push_buff b i x =
  let (bm, buff) = !b in
  b := (Bits.set bm i, x :: buff)
let push_buff_opt b i x =
  let (bm, buff) = !b in
  match x with
  | None -> b := (bm, buff)
  | Some x' -> b := (Bits.set bm i, x' :: buff)

let filter fn v =
  let buff = empty_buff () in
  iter (fun i x -> if fn i x then push_buff buff i x) v;
  reify_buff buff

let filter_map fn v =
  let buff = empty_buff () in
  iter (fun i x -> push_buff_opt buff i (fn i x)) v;
  reify_buff buff

let fold fn v init =
  let acc = ref init in
  iter (fun i x -> acc := fn i x !acc) v;
  !acc

let min_opt v =
  match Bits.min_opt v.bitmap with
  | None -> None
  | Some i -> Some (i, Array.unsafe_get v.storage (Bits.before v.bitmap i))

let iter2 fn_a fn_b fn_ab a b =
  Bits.iter2
    (fun i a_slot -> fn_a i (Array.unsafe_get a.storage a_slot))
    (fun i b_slot -> fn_b i (Array.unsafe_get b.storage b_slot))
    (fun i a_slot b_slot ->
       let av = Array.unsafe_get a.storage a_slot in
       let bv = Array.unsafe_get b.storage b_slot in
       fn_ab i av bv)
    a.bitmap b.bitmap

let merge' map_a map_b merge_ab a b =
  let buff = empty_buff () in
  Bits.iter2
    (fun i a_slot -> push_buff_opt buff i (map_a i (Array.unsafe_get a.storage a_slot)))
    (fun i b_slot -> push_buff_opt buff i (map_b i (Array.unsafe_get b.storage b_slot)))
    (fun i a_slot b_slot ->
       let av = Array.unsafe_get a.storage a_slot in
       let bv = Array.unsafe_get b.storage b_slot in
       push_buff_opt buff i (merge_ab i av bv))
    a.bitmap b.bitmap;
  reify_buff buff

include Mergeable.MergeSet(struct
    type key = int
    type nonrec 'a t = 'a t
    let merge' = merge'
  end)
