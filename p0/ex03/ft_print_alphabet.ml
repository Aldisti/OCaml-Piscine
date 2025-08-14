
let ft_print_alphabet unit : unit =
  for c = 97 to (97 + 26 - 1) do
    print_char (char_of_int c)
  done;
  print_char '\n'

(* TEST SUITE *)

let main unit = ft_print_alphabet ()

let () = main ()
