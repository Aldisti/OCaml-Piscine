
let eu_dist (a: float array) (b: float array) : float =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) a b;
  (!sum ** 0.5)

let print a =
  for i = 0 to ((Array.length a) - 1) do
    print_float a.(i); print_string " "
  done;
  print_newline ()

let test a b =
  print a; print b;
  let res = eu_dist a b in
  print_string "res: "; print_float res; print_newline ()

let () =
  test [|1.5;1.7;2.3|] [|2.1; 2.4; 1.3|]; (* 1.360147 *)
  test [|0.5; 3.6|] [|6.2; 3.5|] (* 5.70088 *)
