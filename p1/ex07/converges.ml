
let rec converges (f: 'a -> 'a) (x:'a) (n:int) : bool =
  if (f x) = x then true
  else if n <= 0 then false
  else converges f (f x) (n - 1)

(* TEST SUITE *)

let () = (
  let test res =
    print_endline (if res then "OK" else "KO")
  in
  test ((converges (( * ) 2) 2 5) = false);
  test ((converges (fun x -> x / 2) 2 3) = true);
  test ((converges (fun x -> x / 2) 2 2) = true);
)
