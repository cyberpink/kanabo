open Kanabo

let random_inserts (module M : Amt.S with type key = int) iters =
  let keys = Array.init iters (fun _ -> Random.int iters) in
  let values = keys in
  let map = ref M.empty in
  
  for i = 0 to iters - 1 do
    let key = keys.(i) in
    let value = values.(i) in
    map := M.add key value !map
  done;

  for i = 0 to iters - 1 do
    let key = keys.(i) in    
    let v = Option.get @@ M.find_opt key !map in
    let expected = values.(i) in
    if expected <> v then
      Printf.printf "random check failed map[%d] %d <> %d\n" i expected v
  done;
  Printf.printf "%d random inserts successful\n" iters

let test_union (module M : Amt.S with type key = int) rand_range =
  let _ = Random.int 2 in
  let make size =
    let m = ref M.empty in
    for _i = 0 to size do
      let k = Random.int size in
      let v = Random.int size in
      m := M.add k [v] !m
    done;
    !m
  in
  let a = make rand_range in
  let b = make rand_range in

  let open Format in
  let print k =
    printf "%d = %a;@ " k @@ fun ppf ->
    fprintf ppf "[%a]" @@
    pp_print_list
      ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
      pp_print_int
  in
  printf "\nA\n-----\n";
  M.iter print a;
  printf "\n\nB\n-----\n";
  M.iter print b;
  printf "\n\nA + B\n-----\n";
  M.iter print @@ M.union (fun _ a b -> Some (List.append a b)) a b;
  print_newline ()
  
module AMT = Amt.AMT
let () = random_inserts (module AMT) 40000
let () = test_union (module AMT) 20

let () =
  let open AMT in
  let t =
    List.fold_left (fun m (k, v) -> add k v m)
      empty
      [(1 lsl 0, 1);
       (1 lsl 4, 10);
       (1 lsl 8, 100);
       (1 lsl 16, 1000);
       (1 lsl 30, 10000)]
  in
  Format.printf "fold: %b\n" @@
  (11111 = AMT.fold (fun _ -> Int.add) t 0)
