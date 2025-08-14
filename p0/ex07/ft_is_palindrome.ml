
let ft_is_palindrome (s: string) : bool =
  let res = ref true in
  let len = String.length s - 1 in
  for i = 0 to len / 2 do
    res := !res && (String.get s i = String.get s (len - i))
  done;
  !res
;;

(* TEST SUITE *)

let test s : unit =
  print_string s;
  if ft_is_palindrome s then
    print_string " => true\n"
  else
    print_string " => false\n"
;;

test "ciao";;
test "ciaoaic";;
test "ciaooaic";;
test "pizza";;
