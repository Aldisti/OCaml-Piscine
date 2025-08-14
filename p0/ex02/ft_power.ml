
let rec ft_power (n: int) (e: int) : int =
  if (e = 0) then 1 else
    if e == 1 then n else (
      n * (ft_power n (e -1))
    )
  ;;

let test n e : unit =
  print_int n;
  print_string " ^ ";
  print_int e;
  print_string " => ";
  print_int (ft_power n e);
  print_char '\n';;

test 2 3;;
test 1 0;;
test 3 4;;
test 0 5;;
