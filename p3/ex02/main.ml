
let bool_to_string = function
  | true -> "true"
  | false -> "false"

let print_card card =
  Card.toString card |> print_string; print_string " "

let test_compare c1 c2 =
  Printf.printf "%s - %s = %d\n" (Card.toString c1) (Card.toString c2) (Card.compare c1 c2)

let test_min_max c1 c2 =
  let c1_s = Card.toString c1 in
  let c2_s = Card.toString c2 in
  Printf.printf "min(%s, %s) => %s - max(%s, %s) => %s\n" c1_s c2_s (Card.min c1 c2 |> Card.toString) c1_s c2_s (Card.max c1 c2 |> Card.toString)

let () =
  let card = Card.newCard Card.Value.Jack Card.Color.Spade in
  Card.toString card |> print_endline;
  Card.toStringVerbose card |> print_endline;

  Card.allSpades |> List.iter print_card; print_newline ();
  Card.allHearts |> List.iter print_card; print_newline ();
  Card.allDiamonds |> List.iter print_card; print_newline ();
  Card.allClubs |> List.iter print_card; print_newline ();

  let queen = Card.newCard Card.Value.Queen Card.Color.Diamond in
  let ace = Card.newCard Card.Value.Ace Card.Color.Club in

  test_compare queen ace;
  test_compare ace queen;
  test_compare ace ace;

  test_min_max ace queen;
  test_min_max queen ace;

  let queen_clubs = Card.newCard Card.Value.Queen Card.Color.Club in

  test_min_max queen_clubs queen;

  Printf.printf "%s is of Spade -> %s\n" (Card.toStringVerbose queen) (Card.isSpade queen |> bool_to_string);
  Printf.printf "%s is of Diamonds -> %s\n" (Card.toStringVerbose queen) (Card.isDiamond queen |> bool_to_string);

  Card.allSpades |> Card.best |> Card.toStringVerbose |> print_endline;
  Card.all |> Card.best |> Card.toStringVerbose |> print_endline;
