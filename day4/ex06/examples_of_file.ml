
type example = float array * string

let read_lines (file : string) : string list =
  In_channel.with_open_text file In_channel.input_lines

let new_example (s: string) : example =
  let fields = String.split_on_char ',' s |> Array.of_list in
  let nums = Array.(sub fields 0 ((length fields) - 1)) in
  Array.((map float_of_string nums, fields.(length fields - 1)))

let examples_of_file filepath : example list =
  read_lines filepath |> List.map new_example


(* TEST *)


let exampleToString (nums, label) =
  let print arr =
    for i = 0 to Array.length arr - 1 do
      Printf.printf "%.2f " arr.(i)
    done
  in
  print nums;
  print_string " => ";
  print_endline label


let main argv =
  if Array.length argv != 2 then print_endline "Error: Invalid number of arguments."
  else
    let examples = examples_of_file argv.(1) in
    List.iter exampleToString examples

let () =
  main Sys.argv
