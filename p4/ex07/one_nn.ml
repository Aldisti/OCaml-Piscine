
let eu_dist (a, _) (b, _) : float =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) a b;
  sqrt !sum


type radar = float array * string


let one_nn (l: radar list) (r: radar) : string =
  let best (d, init) x =
    let dx = eu_dist x r in
    if d < dx then (d, init) else (dx, x)
  in
  match l with
  | [] -> invalid_arg "Cannot calculate 1-nearest with empty list"
  | e :: t -> List.fold_left best (eu_dist e r, e) t |> snd |> snd

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

let main av =
  if Array.length av != 2 then
    print_endline "Error: Invalid number of arguments."
  else
    let examples = examples_of_file av.(1) in
    match examples with
    | [] -> invalid_arg "Error: Empty set of radars."
    | e :: examples ->

    print_endline "Choosen radar:";
    print_radar e;
    print_endline "Radars:";
    List.iter print_radar examples;

    print_string "Tag is: ";
    print_endline @@ one_nn examples e

let () =
  main Sys.argv
