
Random.self_init ()

let rec random_str ?(acc="") (size: int) : string = 
  if size = 0 then acc
  else (
    let c = char_of_int ((Random.int 26) + (
      if acc = "" then 65 else 97)
    ) in random_str ~acc:(acc ^ (Char.escaped c)) (size - 1)
  )

let messages = [|
  "Explain! Explain!";
  "Exterminate! Exterminate!";
  "I obey!";
  "You are the Doctor! You are the enemy of the Daleks!"
|]


class dalek =
  object

  val name : string = "Dalek" ^ random_str 3
  val mutable hp : int = 100
  val mutable shield : bool = true

  method to_string : string =
    Printf.sprintf "Dalek(name: %s, hp: %d, shield: %s)" name hp (string_of_bool shield)

  method talk : unit =
    messages.(Array.length messages |> Random.int) |> print_endline

  method exterminate (p: People.people) : unit =
    p#die; shield <- not shield

  method die : unit =
    hp <- 0;
    print_endline "Emergency Temporal Shift!"

end
