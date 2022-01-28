module Make () = struct
  type t = ..
  let compare : t -> t -> int = compare
  module Variant(T : sig type t end) = struct
    type t += Dyn of T.t
    let inj v = Dyn v
    let prj = function Dyn v -> Some v | _ -> None
  end
end
