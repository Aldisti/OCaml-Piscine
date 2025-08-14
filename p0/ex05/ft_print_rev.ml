
let ft_print_rev (s: string) : unit =
  for i = ((String.length s) - 1) downto 0 do
    print_char (String.get s i)
  done;
  print_char '\n'
;;

let test s r : unit =
  ft_print_rev s;
  print_string r;
  print_char '\n'
;;

test "ciao" "oaic";;
test "" "";;
test "0123456789" "9876543210";;
