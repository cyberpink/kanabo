type t = { width : int; height : int; depth : int; buffer : Bytes.t }

let copy p = { p with buffer = Bytes.copy p.buffer }

let make width height depth =
  let buffer = Bytes.create (width * height * depth * 3) in
  { width; height; depth; buffer }

let width p = p.width
let height p = p.height                   

let get_pixel p x y =
  let get =
    match p.depth with
    | 1 -> Bytes.get_uint8
    | 2 -> Bytes.get_uint16_be
    | _ -> raise @@ Invalid_argument "bad depth"
  in
  let pixel = (p.width * y) + x in
  let get_channel c = get p.buffer (pixel + c) in
  (get_channel 0, get_channel 1, get_channel 2)

let set_pixel p x y (r, g, b) =    
  let set =
    match p.depth with
    | 1 -> Bytes.set_uint8
    | 2 -> Bytes.set_uint16_be
    | _ -> raise @@ Invalid_argument "bad depth"
  in    
  let pixel = (p.width * y) + x in    
  let set_channel c v = set p.buffer (pixel + c) v in
  set_channel 0 r;
  set_channel 1 g;
  set_channel 2 b

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
  fun chan ->
    match really_input_string chan 2 with
    | "P6" ->
      let width = read_header_int chan in
      let height = read_header_int chan in
      let max_val = read_header_int chan in
      let color_bytes =
        match max_val with
        | v when v < 0 -> failwith "invalid color space: negative"
        | v when v = 0 -> failwith "invalid color space: empty"
        | v when v < 256 -> 1
        | v when v < 65536 -> 2
        | _ -> failwith "invalid color space: too large"
      in
      let buffer_size = color_bytes * 3 * width * height in
      let bytes_in = Bytes.create buffer_size in
      really_input chan bytes_in 0 buffer_size;
      { width; height; depth = color_bytes; buffer = bytes_in }
    | _ -> failwith "invalid ppm file: missing magic number"

let print p chan =
  Printf.fprintf chan "P6\n%d\n%d\n%d\n" p.width p.height p.depth;
  for y = 0 to p.height - 1 do
    for x = 0 to p.width - 1 do
      let (r, g, b) = get_pixel p x y in
      Printf.fprintf chan "%d%d%d" r g b
    done
  done
