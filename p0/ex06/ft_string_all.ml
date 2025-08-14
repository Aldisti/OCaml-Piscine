
let ft_string_all (p: char -> bool) (s: string) : bool =
  let i = ref 0 in
    while !i < String.length s && p (String.get s !i) do (
      i := (!i + 1)
    ) done;
  !i = String.length s

(* TEST SUITE *)

let test (p: char -> bool) s : unit =
  if ft_string_all p s then
    print_endline "true" else print_endline "false"

let is_digit = fun c -> c >= '0' && c <= '9'
let is_lower = fun c -> (c >= 'a' && c <= 'z')

let main unit =
  test is_digit "1234";
  test is_digit "12A4";
  test is_digit "";
  test is_lower "ciao";
  test is_lower "COME";
  test is_lower "stAi"

let () = main ()
