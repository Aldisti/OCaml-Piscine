
let rec ft_countdown (n: int) : unit =
    if (n <= 0) then (
        print_int 0;
        print_char '\n';
    )
    else (
        print_int n;
        print_char '\n';
        ft_countdown (n - 1)
    )

(* TEST SUITE *)

let () =
    print_string "----- -42 -----\n";
    ft_countdown (-42);
    print_string "----- 0 -----\n";
    ft_countdown 0;
    print_string "----- 5 -----\n";
    ft_countdown 5
