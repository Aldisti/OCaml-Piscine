
let rec ft_countdown (n: int) : unit =
    if (n < 0) then () else (
        print_int n;
        print_char '\n';
        ft_countdown (n - 1)
    );;

ft_countdown (-42);;
ft_countdown 0;;
ft_countdown 5;;
