
let rec iter (f: int -> int) x n =
  if n < 0 then -1
  else if n = 0 then x
  else if n = 1 then f x
  else iter f (f x) (n - 1)

(* TEST SUITE *)

let () = (
  let print n res =
    Printf.printf "%s - %d\n" (if n = res then "OK" else "KO") n
  in
  print (iter (fun x -> x + 1) (-1) 0) (-1);
  print (iter (fun x -> x + 1) (-1) 1) 0;
  print (iter (fun x -> x + 1) 10 (-1)) (-1);
  print (iter (fun x -> x * x) 2 4) 65536;
  print (iter (fun x -> x * 2) 2 4) 32;
)
