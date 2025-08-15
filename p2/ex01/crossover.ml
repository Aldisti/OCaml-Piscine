
let crossover l1 l2 =
  let rec is_in lst e : bool =
    match lst with
    | [] -> false
    | x :: _ when x = e -> true
    | _ :: tail -> is_in tail e
  in
  let rec filter f lst =
    match lst with
    | [] -> []
    | x :: tail when f x -> x :: filter f tail
    | _ :: tail -> filter f tail
  in
  if l1 = [] || l2 = [] then []
  else filter (fun x -> is_in l2 x) l1

(* TEST SUITE *)

let () =
  let rec print_list f = function
    | [] -> ()
    | [x] -> f x
    | x :: xs -> f x; print_string ", "; print_list f xs
  in
  let print_int_list ?(ends="\n") lst =
    print_string "[";
    print_list print_int lst;
    print_string "]";
    print_string ends
  in
  let test a b =
    print_int_list a ~ends:" ";
    print_int_list b ~ends:" -> ";
    print_int_list (crossover a b)
  in
  test [1; 2; 3] [2; 3];
  test [] [1; 2; 3];
  test [1; 2; 3] [];
  test [1; 2; 3] [4; 5];
  test [1] [2; 3; 1];
  test [] []