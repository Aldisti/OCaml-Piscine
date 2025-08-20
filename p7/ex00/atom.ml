
class virtual atom name symbol number =
  object (self)

  method name : string = name
  
  method symbol : string = symbol
  
  method atomic_number : int = number

  method to_string : string =
    Printf.sprintf "{name: %s, symbol: %s, number: %d}" name symbol number

  method equals (other: atom) : bool =
    self#name = other#name && self#symbol = other#symbol && self#atomic_number = other#atomic_number
end

class hydrogen =
  object
  inherit atom "hydrogen" "H" 1
end

class carbon =
  object
  inherit atom "carbon" "C" 6
end

class oxygen =
  object
  inherit atom "oxygen" "O" 8
end

class sodium =
  object
  inherit atom "sodium" "Na" 11
end

class phosphorus =
  object
  inherit atom "phosphorus" "P" 15
end

class iron =
  object
  inherit atom "iron" "Fe" 26
end
