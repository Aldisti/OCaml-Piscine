
let ft_is_palindrome (s: string) : bool =
  let len = String.length s in
  let check i =
    String.get s i = String.get s (len - 1 - i)
  in
  let rec loop i acc =
    if (i >= len / 2 || not acc) then acc
    else loop (i + 1) (check i && acc)
  in
  loop 0 true

(* TEST SUITE *)

let test s : unit =
  print_string s;
  print_string " => ";
  print_endline (string_of_bool (ft_is_palindrome s))

let () =
  test "ciao";
  test "ciaoaic";
  test "ciaooaic";
  test "pizza";
  test "";
  test "car";
  test "madam";
  test "radar";
  test "123321";
  test "123421"
