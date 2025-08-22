
let encode (lst: 'a list) : (int * 'a) list =
  let rec reverse ?(acc=[]) l =
    match l with
    | [] -> acc
    | e :: t -> reverse ~acc:(e :: acc) t
  in
  let rec test acc (n, curr) lst =
    match lst with
    | [] -> (n, curr) :: acc
    | e :: t ->
      if e = curr then test acc (n + 1, curr) t
      else test ((n, curr) :: acc) (1, e) t
  in
  match lst with
  | [] -> []
  | e :: t -> test [] (1, e) t |> reverse

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
    ['h'; 'h'; 'i'; 'i'; 'i'; 'i'; 'z']
  ]