
let rec sequence (n: int) : string =
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
      let (first, second, third) = count t x 1 in
      generate third (second :: first :: acc)
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
  let test n =
    Printf.printf "%d: %s\n" n (sequence n);
  in
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;
  test 6;
  test 7;
  test 8;
