
let tardis = "     ___________
    / ========= \\
   / ___________ \\
  |  POLICE  BOX  |
  |  ___________  |
  | |  ___ ___  | |
  | | |   |   | | |
  | | |___|___| | |
  | |  ___ ___  | |
  | | |   |   | | |
  | | |___|___| | |
  | |           | |
  | |  _______  | |
  | | |       | | |
  | | |       | | |
  | | |_______| | |
  | |___________| |
  |_______________|
"

class doctor name age sidecick =
  object (self)

  val name : string = name
  val mutable age : int = age
  val mutable hp : int = 100
  val _sidecick : People.people = sidecick

  initializer Printf.printf "Creating doctor %s, age: %d, hp: %d and sidecick: %s\n" name age hp (_sidecick#to_string)

  method to_string : string =
    Printf.sprintf "Doctor(name: %s, age: %d, hp: %d, sidecick: %s)" name age hp (_sidecick#to_string)

  method talk : unit = print_endline "Hi! I'm the Doctor!"

  method travel_in_time (start: int) (arrival: int) : unit =
    print_endline tardis

  method use_sonic_screwdriver =
    print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii";
    self#regenerate

  method take_damage dmg =
    if hp < dmg then hp <- 0
    else hp <- hp - dmg

  method private regenerate = hp <- 100

end
