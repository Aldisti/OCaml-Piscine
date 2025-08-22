
module type Set = sig
  type 'a t = 'a list

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val filter : 'a t -> ('a -> bool) -> 'a t
  val foreach : 'a t -> ('a -> unit) -> unit
  val for_all : 'a t -> ('a -> bool) -> bool
  val exists : 'a t -> ('a -> bool) -> bool
  val to_string : 'a t -> ('a -> string) -> string
end

module Set: Set = struct
  type 'a t = 'a list

  let return x = [x]

  let bind s f =
    let rec loop acc lst =
      match lst with
      | [] -> acc
      | e :: t -> loop (acc @ f e) t
    in
    loop [] s

  let union s1 s2 = s1 @ s2

  let inter s1 s2 =
    match (s1, s2) with
    | (_, []) | ([], _) -> []
    | _ -> List.(
      filter (fun x -> exists (fun y -> x = y) s2) s1
    )

  let diff s1 s2 =
    let oneway_diff s1 s2 =
      List.(filter (fun x -> for_all (fun y -> x <> y) s2) s1)
    in
    match (s1, s2) with
    | (_, []) -> s1
    | ([], _) -> s2
    | _ -> (oneway_diff s1 s2) @ (oneway_diff s2 s1)

  let filter s f = List.filter f s
  let foreach s f = List.iter f s
  let for_all s f = List.for_all f s
  let exists s f = List.exists f s

  let to_string s f =
    "[" ^ (List.map f s |> String.concat ", ") ^ "]"
end

let () =
  let s1: int Set.t = [1; 2; 3; 4] in
  Printf.printf "s1 (return): %s\n" (Set.to_string s1 string_of_int);

  let s2 = Set.bind s1 (fun x -> Set.return (x + 4)) in
  Printf.printf "s2 (bind s1): %s\n" (Set.to_string s2 string_of_int);

  let s3 = Set.union s1 s2 in
  Printf.printf "s3 (union s1 s2): %s\n" (Set.to_string s3 string_of_int);

  let s4 = Set.inter s3 s1 in
  Printf.printf "s4 (inter s3 s1): %s\n" (Set.to_string s4 string_of_int);

  let s5 = Set.diff s1 s3 in
  Printf.printf "s5 (diff s1 s3): %s\n" (Set.to_string s5 string_of_int);

  let s6 = Set.filter s3 (fun x -> x > 2 && x < 5) in
  Printf.printf "s6 (filter s3): %s\n" (Set.to_string s6 string_of_int);

  print_string "(foreach s5): ";
  Set.foreach s5 (fun x -> Printf.printf "(%d) " x);
  print_newline ();

  Printf.printf "(for_all s1 >0): %b\n" (Set.for_all s1 (fun x -> x > 0));
  Printf.printf "(for_all s1 <3): %b\n" (Set.for_all s1 (fun x -> x < 3));

  Printf.printf "(exists s1 2): %b\n" (Set.exists s1 (fun x -> x = 1));
  Printf.printf "(exists s1 5): %b\n" (Set.exists s1 (fun x -> x = 5));
