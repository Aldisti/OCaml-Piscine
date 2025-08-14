
let rec repeat_x (n: int) : string =
  if n < 0 then "Error"
  else if n == 0 then ""
  else "x" ^ repeat_x (n - 1)

(* TEST SUITE *)

let () = (
  let test = fun n -> (
    let s = repeat_x n in
    print_int n;
    print_string (" => |" ^ s ^ "| len: ");
    print_int (String.length s);
    print_char '\n'
  ) in
  test 5;
  test 0;
  test (-1);
  test 10
)
