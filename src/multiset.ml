module Sum = struct include Int include Int.Sum end
module Make(O : Stdlib.Map.OrderedType) = struct
  module M = Map.Make(O)
  type t = Int.t M.t
  let mem = M.mem
  let is_empty = M.is_empty
  let empty = M.empty
  let singleton x c = M.singleton x c
  let unit x = singleton x 1

  let find_opt = M.find_opt
  let add x n s = M.update x Sum.(fun x -> to_opt @@ of_opt x + n) s
  let remove x n s = M.update x Sum.(fun x -> to_opt @@ of_opt x - n) s
  let union a b = M.union (fun _ a b -> Some (a + b)) a b        
  let sub = M.merge Sum.(fun _ a b -> to_opt @@ of_opt a - of_opt b)
  let intersect = M.merge Sum.(fun _ a b -> to_opt @@ min (of_opt a) (of_opt b))
  let filter p s = M.filter (fun k _ -> p k) s
  let fold = M.fold
  let iter = M.iter
  let of_seq = Seq.fold_left (fun m (x, i) -> add x i m) empty
  let to_seq = M.to_seq
  let compare = M.compare

  let diff a b =
    let shared = intersect a b in
    (sub a shared, shared, sub b shared)
end
