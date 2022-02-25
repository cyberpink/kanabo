type t = int
type key = int
type elt = int

let empty = 0
let is_empty s = Int.equal s 0
let max_len = Sys.int_size / 8

let init size f =
  let buf = ref 0 in
  for i = size - 1 downto 0 do
    buf := (!buf lsl 8) lor (f i)
  done;
  !buf

let remove s i = s land (lnot (0xFF lsl (i * 8)))
let set s i b = (remove s i) lor (b lsl (i * 8))
let get s i = (s lsr (i * 8)) land 0xFF
