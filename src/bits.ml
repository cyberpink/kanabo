external popcount : int -> int =
  "popcount"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external ctz : (int[@untagged]) -> (int[@untagged]) =
  "ctz" "ctz_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external clz : int -> (int[@untagged]) =
  "clz" "clz_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let popcount32 x =
  let x = x - ((x lsr 1) land 0x55555555) in
  let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
  let x = (x + (x lsr 4)) land 0x0f0f0f0f in
  let x = (x * 0x01010101) lsr 24 in
  x land 0x3f

let before bm i = popcount @@ (bm lsr i) land (lnot 0b1) [@@inline always]
let get bm i = (bm lsr i) land 0b1 [@@inline always]
let set bm i = bm lor (0b1 lsl i) [@@inline always]
let unset bm i = bm land (lnot @@ 0b1 lsl i) [@@inline always]
let flip bm i = bm lxor (0b1 lsl i) [@@inline always]
let min_opt bm = if bm = 0 then None else Some (ctz bm)

let max_opt =
  let off = Sys.int_size - 1 in
  function
  | 0 -> None
  | bm -> Some (off - clz bm)

let unset_lsb n = n land (n - 1) [@@inline always]

let compare = Int.compare

let get_next bm =
  let idx = ctz bm in
  bm lxor (0b1 lsl idx), idx
[@@inline always]

let indexes bm =
  let len = popcount bm in
  let out = Bytes.create len in
  let bm = ref bm in
  for i = len - 1 downto 0 do
    let (bm', idx) = get_next !bm in
    bm := bm';
    Bytes.set_int8 out i idx
  done;
  out

let iter fn bm =
  let rec loop bm =
    if bm <> 0 then
      let (bm', idx) = get_next bm in
      fn idx;
      loop bm'
  in loop bm

let iteri fn bm =
  let len = popcount bm in
  let bm = ref bm in
  for i = len - 1 downto 0 do
    let (bm', idx) = get_next !bm in
    bm := bm';
    fn i idx
  done

let fold fn bm init =
  let len = popcount bm in
  let rec loop i bm m =
    if i > 0 then
      let (bm', idx) = get_next bm in
      loop (pred i) bm' (fn idx i m)
    else
      m
  in loop (len - 1) bm init

let to_seqi bm =  
  let len = popcount bm in
  let rec loop i bm () =
    if i > 0 then
      let (bm', idx) = get_next bm in
      Seq.Cons ((idx, i), loop (pred i) bm')
    else
      Seq.Nil
  in loop (len - 1) bm

let iter2 fn_a fn_b fn_ab a b =
  let a_len = popcount a in
  let b_len = popcount b in
  let (a, a_idx) = get_next a in
  let (b, b_idx) = get_next b in
  let rec loop a a_idx a_slot b b_idx b_slot =
    if a_slot < 0 && b_slot < 0 then
      ()
    else if a_idx > b_idx then
      let () = fn_b b_idx b_slot in
      let (b', b_idx') = get_next b in
      loop a a_idx a_slot b' b_idx' (pred b_slot)
    else if a_idx < b_idx then
      let () = fn_a a_idx a_slot in
      let (a', a_idx') = get_next a in
      loop a' a_idx' (pred a_slot) b b_idx b_slot
    else
      let () = fn_ab a_idx a_slot b_slot in
      let (a', a_idx') = get_next a in
      let (b', b_idx') = get_next b in
      loop a' a_idx' (pred a_slot) b' b_idx' (pred b_slot)
  in loop a a_idx (pred a_len) b b_idx (pred b_len)

module Set = struct
  type nonrec t = int
  let empty = 0
  let cardinal = popcount
  let singleton i = 1 lsl i
  let mem i s = s land (1 lsl i) <> 0
  let add i s = s lor (1 lsl i)
  let rem i s = s land (lnot (1 lsl i))
  let union = (lor)
  let xor = (lxor)
  let intersect = (land)
  let sub a b = a lxor (a land b)
end
