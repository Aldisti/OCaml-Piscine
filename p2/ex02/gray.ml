
let gray n =
  let rec reverse ?(acc=[]) lst = match lst with
    | [] -> acc
    | [e] -> e :: acc
    | e :: t -> reverse ~acc:(e :: acc) t
  in
  let rec map f lst = match lst with
    | [] -> []
    | e :: t -> (f e) :: (map f t)
  in
  let merge l1 l2 =
    let rec internal l1 acc =
      match l1 with
      | [] -> acc
      | e :: t -> internal t (e :: acc)
    in
    if l2 = [] then l1
    else internal (reverse l1) l2
  in
  let rec print_list lst =
    match lst with
    | [] -> print_string "\n"
    | [e] -> print_string (e ^ "\n")
    | e :: tail -> print_string (e ^ " "); print_list tail
  in
  let rec gray_list n =
    match n with
    | 0 -> []
    | 1 -> ["0"; "1"]
    | _ -> (
      let main = gray_list (n - 1) in
      let left = map (fun x -> "0" ^ x) main in
      let right = map (fun x -> "1" ^ x) (reverse main) in
      merge left right
    )
  in
  gray_list n |> print_list

(* TEST SUITE *)

let () =
  let test n =
    print_int n;
    print_string " -> ";
    gray n;
  in
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
