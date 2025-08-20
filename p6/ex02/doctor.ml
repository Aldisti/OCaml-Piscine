
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

  val _name : string = name
  val mutable _age : int = age
  val mutable _hp : int = 100
  val _sidecick : People.people = sidecick

  initializer Printf.printf "Creating doctor %s, age: %d, hp: %d and sidecick: %s\n" _name _age _hp (_sidecick#to_string)

  method to_string : string =
    Printf.sprintf "{name: %s, age: %d, hp: %d, sidecick: %s}" _name _age _hp (_sidecick#to_string)

  method talk : unit = print_endline "Hi! I'm the Doctor!"

  method travel_in_time (start: int) (arrival: int) : unit =
    let new_age = _age + (arrival - start) in
    if new_age <= 0 then
      print_endline "Cannot use the tardis!"
    else (
      _age <- new_age; print_endline tardis
    )

  method use_sonic_screwdriver =
    print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii";
    self#regenerate

  method take_damage dmg =
    if _hp < dmg then _hp <- 0
    else _hp <- _hp - dmg

  method private regenerate = _hp <- 100

end
