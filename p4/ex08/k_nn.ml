
let eu_dist (a, _) (b, _) : float =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) a b;
  sqrt !sum


type radar = float array * string


let k_nn (l: radar list) (n: int) (r: radar) : string =
  let arr = List.map (fun e -> (eu_dist e r, e)) l |> Array.of_list in
  Array.fast_sort (fun (a, _) (b, _) -> Float.compare a b) arr;
  let k = min n (Array.length arr) in
  let smallest = Array.sub arr 0 k in

  let table = Hashtbl.create k in

  Array.iter (fun (_, (_, label)) -> (
    match (Hashtbl.find_opt table label) with
    | None -> Hashtbl.add table label 1
    | Some x -> Hashtbl.replace table label (x + 1)
  )) smallest;

  Hashtbl.fold (fun k v init -> (
    if v > snd init then (k, v) else init
  )) table ("?", 0) |> fst


let new_radar (s: string) : radar =
  let fields = String.split_on_char ',' s |> Array.of_list in
  let nums = Array.(sub fields 0 ((length fields) - 1)) in
  Array.(map float_of_string nums, fields.(length fields - 1))


let examples_of_file filepath : radar list =
  let lines = In_channel.(with_open_text filepath input_lines) in
  List.map new_radar lines


(* TEST *)

let print_radar (nums, label) =
  let print arr =
    for i = 0 to Array.length arr - 1 do
      Printf.printf "%.2f," arr.(i)
    done
  in
  print nums;
  print_endline label


let main ac av =
  if ac < 3 || ac > 5 then
    print_endline "Error: usage ./k_nn <train.csv> <test.csv> [min k] [max k]."
  else
    let train = examples_of_file av.(1) in
    let tests = examples_of_file av.(2) in
    let sk = if ac > 3 then int_of_string av.(3) else 1 in
    let ek = if ac > 4 then int_of_string av.(4) else sk in
    let test e =
      print_string "Choosen radar: "; snd e |> print_endline;
      for i = sk to ek do
        let tag = k_nn train i e in
        Printf.printf "Tag for k=%d is %s\n" i tag
      done
    in
    List.iter (fun r -> test r) tests


let () =
  main (Array.length Sys.argv) Sys.argv
