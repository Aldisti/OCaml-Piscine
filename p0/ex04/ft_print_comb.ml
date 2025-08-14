
let ft_print_comb unit : unit =
  for x = 0 to 7 do (
    for y = (x + 1) to 8 do (
      for z = (y + 1) to 9 do (
        print_int x; print_int y; print_int z;
        if x <> 7 then print_string ", " else ()
      ) done
    ) done
  ) done;
  print_string "\n"
;;

ft_print_comb ();;
