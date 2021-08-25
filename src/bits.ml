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

let iteri fn bm =
  let len = popcount bm in
  let bm = ref bm in
  for i = len - 1 downto 0 do
    let idx = ctz !bm in      
    bm := !bm lxor (0b1 lsl idx);
    fn idx i
  done

