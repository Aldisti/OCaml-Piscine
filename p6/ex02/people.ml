
class people name =
  object (self)

  val name : string = name
  val mutable hp : int = 100

  initializer Printf.printf "Creating new people: %s\n" name

  method to_string : string =
    Printf.sprintf "People(%s, %d)" name hp

  method talk : unit =
    Printf.printf "I'm %s! Do you know the Doctor?\n" name

  method die : unit =
    hp <- 0; Printf.printf "Aaaarghh!\n"

end
