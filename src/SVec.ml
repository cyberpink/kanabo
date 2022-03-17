(* Sparse Vectors using bitmaps 
   Maximum size of 31 or 63 depending on arch
   
   TODO: parameterize storage incase bytevector wanted
*)
type key = int
type 'a t = { bitmap : int; storage : 'a array }
let empty = { bitmap = 0; storage = Array.empty }
let is_empty v = v.bitmap = 0
let length v = Array.length v.storage
let mem v i = Bits.get v.bitmap i <> 0      
let find_opt v i =
  if mem v i
  then Some (Array.unsafe_get v.storage (Bits.before v.bitmap i))
  else None

let empty_buff () = ref (0, [])
let reify_buff b =
  let (bm, buff) = !b in
  if bm = 0 then empty else { bitmap = bm; storage = Array.of_list buff }
let push_buff b i x =
  let (bm, buff) = !b in
  match x with
  | None -> b := (bm, buff)
  | Some x' -> b := (Bits.set bm i, x' :: buff)

let update v i fn =
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

let remove v i = update v i (fun _ -> None)
let add v i x = update v i (fun _ -> Some x)
let map fn v = { v with storage = Array.map fn v.storage }
let iter fn v = Array.iter fn v.storage
let iteri fn v = Bits.iteri (fun idx i -> fn idx @@ Array.unsafe_get v.storage i) v.bitmap
let filter_mapi fn v =
  let buff = empty_buff () in
  iteri (fun i x -> push_buff buff i (fn i x)) v;
  reify_buff buff

let fold fn v init =
  let acc = ref init in
  iteri (fun i x -> acc := fn i x !acc) v;
  !acc

let min v =
  Option.bind (Bits.min_opt v.bitmap) @@
  fun i -> Some (i, Array.get v.storage i)

let merge map_a map_b merge_ab a b =
  let buff = empty_buff () in
  Bits.iter2
    (fun i a_slot -> push_buff buff i (map_a i (Array.unsafe_get a.storage a_slot)))    
    (fun i b_slot -> push_buff buff i (map_b i (Array.unsafe_get b.storage b_slot)))
    (fun i a_slot b_slot ->
       let av = Array.unsafe_get a.storage a_slot in
       let bv = Array.unsafe_get b.storage b_slot in
       push_buff buff i (merge_ab i av bv))
    a.bitmap b.bitmap;
  reify_buff buff


include Mergeable.MergeSet(struct
    type key = int
    type nonrec 'a t = 'a t
    let merge = merge
  end)
