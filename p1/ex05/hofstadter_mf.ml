
let rec hfs_f ?(def=1) n : int =
  let def_m = if def == 1 then 0 else 1 in
  if n < 0 then -1
  else if n <= 1 then def
  else (n - (hfs_f ~def:def_m (hfs_f ~def:def (n - 1))))

let rec hfs_m ?(def=0) n : int =
  let def_f = if def == 0 then 1 else 0 in
  if n < 0 then -1
  else if n <= 1 then def
  else (n - (hfs_m ~def:def_f (hfs_m (n - 1))))


(*
F(n>0) = n - M(F(n - 1))
M(n>0) = n - F(M(n - 1))

F(0) = 1
M(0) = 0

F(1) = 1
M(1) = 0

F(2) = 2
M(2) = 1

F(3) = 2
M(3) = 2

F(4) = 3
M(4) = 2
*)

(* TEST SUITE *)

let () = (

  let test n f_res m_res =
    let print n actual res = 
      Printf.printf "%s - %d = %d\n" (if actual = res then "OK" else "KO") n actual
    in
    print n (hfs_f n) f_res;
    print n (hfs_m n) m_res;
  in
  test 0 1 0;
  test 1 1 0;
  test 2 2 1;
  test 3 2 2;
  test 4 3 2;
)
