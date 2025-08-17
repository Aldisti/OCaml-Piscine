
let () =
  let deck = Deck.newDeck () in
  let other_deck = Deck.newDeck () in

  Printf.printf "deck.length: %d other_deck.length: %d\n" (List.length deck) (List.length other_deck);

  let print2 a b = Printf.printf "%s <--> %s\n" a b in

  List.iter2 print2 (Deck.toStringList deck) (Deck.toStringList other_deck);

  let (top_card, deck_tail) = Deck.drawCard deck in
  Printf.printf "Extracted card: %s\n" (Deck.Card.toStringVerbose top_card);

  Printf.printf "deck_tail.length: %d\n" (List.length deck_tail);

  try ignore (Deck.drawCard []) with
  | Failure msg -> Printf.printf "Exception: %s\n" msg

