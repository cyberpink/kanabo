external popcount : int -> int = "popcount" [@@noalloc]
external ctz : (int[@untagged]) -> (int[@untagged]) = "ctz" "ctz_untagged" [@@noalloc]
external clz : int -> (int[@untagged]) = "clz" "clz_untagged" [@@noalloc]

let popcount32 x =
  let x = x - ((x lsr 1) land 0x55555555) in
  let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
  let x = (x + (x lsr 4)) land 0x0f0f0f0f in
  let x = (x * 0x01010101) lsr 24 in
  x land 0x3f

let before bm i = popcount @@ (bm lsr i) land (lnot 0b1)
let get bm i = (bm lsr i) land 0b1
let set bm i = bm lor (0b1 lsl i)
let unset bm i = bm land (lnot @@ 0b1 lsl i)
let flip bm i = bm lxor (0b1 lsl i)

let min_opt bm = if bm = 0 then None else Some (ctz bm)

let get_next bm =
  let idx = ctz bm in
  bm lxor (0b1 lsl idx), idx
[@@inline always]

let iter fn bm =
  let rec loop bm =
    if bm != 0 then
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
    fn idx i
  done

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
    
  

