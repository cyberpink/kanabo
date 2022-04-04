include Stdlib.Map

module type S = sig
  include Stdlib.Map.S
  include module type of HK.Make(struct type nonrec 'a t = 'a t end)
end

module Make(O : OrderedType) = struct
  include Stdlib.Map.Make(O)
  include HK.Make(struct type nonrec 'a t = 'a t end)

  let find' k d m =
    match find_opt k m with
    | None -> Lazy.force d
    | Some v -> v

  let update' k f d m =
    update k
      (function None -> f (Lazy.force d) | Some v -> f v)
      m
end
