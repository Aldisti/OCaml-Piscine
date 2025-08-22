
let jokes = [|
  "What did an orange say to another orange? Nothing, oranges don't talk.";
  "Why don't oysters donate to charity? Because they're shellfish.";
  "What does a baby computer call its father? Data.";
  "What do you call a fish with no eye? Fsh.";
  "Did you hear about the claustrophobic astronaut? Poor guy really needed some space.";
  "I'm only familiar with 25 letters of the alphabet. I don't know why.";
  "What do you call a deer with no eyes? No eyed deer."
|]

let get_random arr : string =
  arr.(Array.length arr |> Random.int)

let () =
  Random.self_init ();
  get_random jokes |> print_endline
