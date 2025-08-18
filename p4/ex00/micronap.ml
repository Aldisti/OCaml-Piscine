
(* run with `ocaml -I +unix unix.cma micronap.ml <int>` *)

let my_sleep () = Unix.sleep 1

let second = function
  | _ :: e :: _ -> e
  | _ -> raise (Failure "List must contain at least 2 elements!")

let wait n : unit =
  for i = 0 to n do
    my_sleep ()
  done

let main argc argv =
  if argc != 2 then print_endline "Error: Invalid number of arguments!"
  else
    try
      second argv |> int_of_string |> wait
    with
    | Failure msg -> print_string "Error: "; print_endline msg

let () =
  let argv = Array.to_list Sys.argv in
  main (List.length argv) argv

