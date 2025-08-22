
let read_file filename : string array =
  let final = ref [||] in
  let channel = open_in filename in
  try
    while true do
      final := Array.append !final [|input_line channel|]
    done
  with End_of_file -> close_in channel;
  !final

let get_random arr =
  arr.(Array.length arr |> Random.int)

let main = function
  | [|_; e|] -> read_file e |> get_random |> print_endline
  | _ -> print_endline "Error: Invalid number of arguments."

let () =
  Random.self_init ();
  main Sys.argv
