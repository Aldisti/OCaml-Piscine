
let rec sequence n =
  let first (a, _, _) = a in
  let second (_, b, _) = b in
  let third (_, _, c) = c in
  let rec generate lst acc =
    let rec count lst curr c =
      match lst with
      | [] -> (c, curr, lst)
      | e :: t when e <> curr -> (c, curr, lst)
      | e :: t -> count t curr (c + 1)
    in
    match lst with
    | [] -> acc
    | x :: t ->
      let tpl = count t x 1 in
      generate (third tpl) (second tpl :: first tpl :: acc)
  in
  let rec reverse ?(acc=[]) l =
    match l with
    | [] -> acc
    | e :: t -> reverse t ~acc:(e :: acc)
  in
  let rec internal n =
    if n <= 0 then []
    else if n == 1 then [1]
    else generate (internal (n - 1)) [] |> reverse
  in
  let rec join = function
    | [] -> ""
    | e :: t -> string_of_int e ^ (join t)
  in
  internal n |> join

(* TEST SUITE *)

let () =
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7)
