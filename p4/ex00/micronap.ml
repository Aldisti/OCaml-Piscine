
(* run with `ocaml -I +unix unix.cma micronap.ml <int>` *)
(* compile with `ocamlopt -I +unix unix.cmxa micronap.ml` *)

let my_sleep () = Unix.sleep 1

let second = function
  | _ :: e :: _ -> e
  | _ -> raise (Failure "List must contain at least 2 elements!")

let wait n : unit =
  for i = 0 to n - 1 do
    my_sleep ()
  done

let main = function
  | [|_; e|] -> e |> int_of_string |> wait
  | _ -> print_endline "Error: Invalid number of arguments!"

let () =
  main Sys.argv

