include Stdlib.Array
let empty = [||]

let remove a i =
  match length a - 1 with
  | 0 -> empty
  | len' ->
    let a' = sub a 0 len' in
    blit a (i + 1) a' i (len' - i);
    a'

let copy_set a i x = let a' = copy a in set a' i x; a'

let splice_at a i x =
  let a' = append [| Obj.magic () |] a in
  blit a 0 a' 0 i;
  set a' i x;
  a'

let get_opt a i =
  try Some (get a i) with
    Invalid_argument _ -> None
    
