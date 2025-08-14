
let ft_is_palindrome (s: string) : bool =
  let rec loop = fun i len -> (
    if i >= len / 2 then true else (
      (String.get s i = String.get s (len - 1 - i)) && loop (i + 1) len
    )
  ) in
  loop 0 (String.length s)

(* TEST SUITE *)

let test s : unit =
  print_string s;
  if ft_is_palindrome s then
    print_string " => true\n"
  else
    print_string " => false\n"

let main unit =
  test "ciao";
  test "ciaoaic";
  test "ciaooaic";
  test "pizza"

let () = main ()
