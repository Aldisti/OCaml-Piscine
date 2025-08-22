
let rec iter f = function
  | [] -> ()
  | e::t -> f e; iter f t

let () =
  iter (fun x -> Color.toString x |> print_string) Color.all;
  print_newline ();
  iter (fun x -> Color.toStringVerbose x |> print_string) Color.all;
  print_newline ()
