
class ['a] army = object

  val mutable _members = ([] : 'a list)

  method add (member: 'a) : unit =
    _members <- member :: _members

  method delete : unit =
    match _members with
    | [] -> ()
    | _ -> _members <- List.tl _members

  method get_members : 'a list = _members

end
