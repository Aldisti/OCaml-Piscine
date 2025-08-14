
let rec fibonacci ?(acc=0) ?(b=1) (n: int) : int =
  if n < 0 then -1
  else if n == 0 then acc
  else if n == 1 then b
  else fibonacci ~acc:b ~b:(acc + b) (n - 1)

(* TEST SUITE *)

let () = (
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
)
