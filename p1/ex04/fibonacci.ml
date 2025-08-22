
let rec fibonacci (n: int) : int =
  let rec loop acc b n =
    if n = 0 then acc
    else if n = 1 then b
    else loop b (acc + b) (n - 1)
  in
  if n < 0 then -1
  else loop 0 1 n

(* TEST SUITE *)

let () =
  let test n res =
    let actual = fibonacci n in
    Printf.printf "%s - %d => %d\n" (if actual = res then "OK" else "KO") n actual
  in
  test (-42) (-1);
  test 0 0;
  test 1 1;
  test 2 1;
  test 3 2;
  test 9 34;
  test 15 610;
