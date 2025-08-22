
let ft_print_rev (s: string) : unit =
  let rec loop i j = (
    if i < j then ()
    else (
      print_char (String.get s i);
      loop (i - 1) j
    )
  ) in
  loop (String.length s - 1) 0;
  print_char '\n'

(* TEST SUITE *)

let test s r : unit =
  ft_print_rev s;
  print_string r;
  print_char '\n'

let () =
  test "ciao" "oaic";
  test "0123456789" "9876543210";
  test "Hello world !" "! dlrow olleH";
  test "" ""
