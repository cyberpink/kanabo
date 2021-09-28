type 'a t

module Make(S:sig type 'a t end) = struct
  type hk
  external inj : 'a S.t -> ('a -> hk) t = "%identity"
  external prj : ('a -> hk) t -> 'a S.t = "%identity"
end

module Make2(S:sig type ('a, 'b) t end) = struct
  type hk
  external inj : ('a, 'b) S.t -> ('a -> 'b -> hk) t = "%identity"
  external prj : ('a -> 'b -> hk) t -> ('a, 'b) S.t = "%identity"
end
