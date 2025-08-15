
let encode lst  =
  let first (a, _) = a in
  let second (_, b) = b in
  let rec reverse ?(acc=[]) l =
    match l with
    | [] -> acc
    | e :: t -> reverse t ~acc:(e :: acc)
  in
  let rec count c curr tail =
    match tail with
    | [] -> (c, tail)
    | x :: t ->
      if x <> curr then (c, tail)
      else count (c + 1) curr t
  in
  let rec counter acc l =
    match l with
    | [] -> acc
    | x :: t ->
      let pair = count 1 x t in
      counter (((first pair), x) :: acc) (second pair)
  in
  if lst = [] then []
  else reverse (counter [] lst)

(* TEST SUITE *)

let () =
  let rec print_list f = function
    | [] -> ()
    | [x] -> f x
    | x :: xs -> f x; print_string "; "; print_list f xs
  in
  let print_char_list lst =
    print_string "[";
    print_list (fun c -> print_char '\''; print_char c; print_char '\'') lst;
    print_string "]"
  in
  let print_encoded lst =
    print_string "[";
    print_list (fun (n, c) -> print_int n; print_string ","; print_char c) lst;
    print_string "]"
  in
  let rec run_tests = function
    | [] -> ()
    | case :: rest ->
        print_char_list case;
        print_string " -> ";
        print_encoded (encode case);
        print_newline ();
        run_tests rest
  in
  run_tests [
    ['a'; 'a'; 'a'; 'b'; 'b'; 'c'];
    [];
    ['x'];
    ['h'; 'h'; 'i'; 'i'; 'i'; 'i']
  ]