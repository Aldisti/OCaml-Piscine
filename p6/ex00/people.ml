
class people name =
  object (self)

  val _name : string = name
  val mutable _hp : int = 100

  initializer Printf.printf "Creating new people: %s\n" _name

  method to_string : string =
    Printf.sprintf "People(%s, %d)" _name _hp

  method talk : unit =
    Printf.printf "I'm %s! Do you know the Doctor?\n" _name

  method die : unit =
    Printf.printf "Aaaarghh!\n"
end
