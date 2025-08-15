
let () =
	print_endline "Get card int value:";
	List.iter (fun value -> print_int (Value.toInt value) ; print_char ' ') Value.all;
  print_newline ();

	print_endline "Value.toString:";
	List.iter (fun value -> print_string (Value.toString value); print_char ' ') Value.all;
  print_newline ();

	print_endline "Value.toStringVerbose:";
	List.iter (fun value -> print_string (Value.toStringVerbose value); print_char ' ') Value.all;
  print_newline ();

	print_endline "Value.next: ";
	print_endline (Value.toStringVerbose (Value.next Value.Queen));
  try ignore(Value.next Value.Ace) with | Invalid_argument msg -> print_endline msg;

	print_endline "Value.previous: ";
	print_endline (Value.toStringVerbose (Value.previous Value.Queen));
  try ignore(Value.previous Value.T2) with | Invalid_argument msg -> print_endline msg

