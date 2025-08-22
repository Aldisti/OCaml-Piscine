
class ['a] army = object

  val mutable members = ([] : 'a list)

  method add (member: 'a) : unit =
    members <- member :: members

  method delete : unit =
    match members with
    | [] -> ()
    | _ -> members <- List.tl members

  method get_members : 'a list = members

end
