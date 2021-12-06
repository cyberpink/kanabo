type 'a pixel = 'a * 'a * 'a
                
type t = {
  width : int;
  height : int;
  max_val : int;
  format : (module Bytes_view.S);
  buffer : Bytes.t
}

let get_format : int -> (module Bytes_view.S) = function
  | v when v < 0 -> invalid_arg "invalid ppm: maximum color value negative"
  | v when v = 0 -> invalid_arg "invalid ppm: maximum color value is 0"
  | v when v < 256 -> (module Bytes_view.U8)
  | v when v < 65536 -> (module Bytes_view.U16_be)
  | _ -> invalid_arg "invalid ppm: maximum color value too large"

let create width height max_val =
  let format = get_format max_val in  
  let module Buf = (val format) in
  let size = width * height * Buf.elt_size * 3 in
  let buffer = Bytes.create size in  
  { width; height; max_val; format; buffer }
  
let copy p = { p with buffer = Bytes.copy p.buffer }
let width p = p.width
let height p = p.height                   

let get_pixel (p : t) x y =
  let module P = (val p.format) in
  let pixel = ((p.width * y) + x) * 3 in
  let buf = P.of_bytes p.buffer in
  P.(get buf (pixel + 0),
     get buf (pixel + 1),
     get buf (pixel + 2))

let set_pixel (p : t) x y (r, g, b) =  
  let module P = (val p.format) in
  let pixel = ((p.width * y) + x) * 3 in
  let buf = P.of_bytes p.buffer in
  P.(set buf (pixel + 0) r;
     set buf (pixel + 1) g;
     set buf (pixel + 2) b)

let read =
  let is_whitespace = function
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false
  in
  let rec read_header_int chan =
    match input_char chan with
    (* skip whitespace *)
    | c when is_whitespace c -> read_header_int chan
    (* skip comments *)
    | '#' ->
      let rec loop () =
        match input_char chan with
        | '\r' | '\n' -> read_header_int chan
        | _ -> loop ()
      in loop ()
    | c ->
      let buffer = Buffer.create 8 in
      let rec loop = function
        | c when is_whitespace c -> 
          int_of_string @@ Buffer.contents buffer
        | c ->
          Buffer.add_char buffer c;
          loop (input_char chan)
      in loop c
  in 
  fun (chan : in_channel) ->
    match really_input_string chan 2 with
    | "P6" ->
      let width = read_header_int chan in
      let height = read_header_int chan in
      let max_val = read_header_int chan in
      let pixbuf = create width height max_val in
      really_input chan pixbuf.buffer 0 (Bytes.length pixbuf.buffer);
      pixbuf
    | _ -> invalid_arg "invalid ppm: missing magic number"

let print p chan =
  let module Pixbuf = (val p.format) in
  Printf.fprintf chan "P6\n%d %d\n%d\n" p.width p.height p.max_val;
  output_bytes chan p.buffer
