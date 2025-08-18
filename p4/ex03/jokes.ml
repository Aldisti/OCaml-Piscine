
let read_file filename : string array =
  let final = ref [||] in
  let channel = open_in filename in
  try
    while true do
      final := Array.append !final [|input_line channel|]
    done
  with End_of_file -> close_in channel;
  !final

let print arr =
  for i = 0 to ((Array.length arr) - 1) do
    print_endline arr.(i)
  done

let get_random arr =
  Random.self_init ();
  arr.(Array.length arr |> Random.int)

let main argv =
  if Array.length argv != 2 then
    print_endline "Error: Invalid number of arguments."
  else
    let jokes = read_file argv.(1) in
    get_random jokes |> print_endline

let () =
  main Sys.argv
