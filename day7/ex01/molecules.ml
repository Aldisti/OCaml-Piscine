
class virtual molecule name (atoms: Atoms.atom list) = object (self)

  method name : string = name

  method formula =
    let table = Hashtbl.create (List.length atoms) in
    List.iter (fun x -> (
      match Hashtbl.find_opt table x#symbol with
      | None -> Hashtbl.add table x#symbol (x, 1)
      | Some (s, e) -> Hashtbl.replace table x#symbol (x, e + 1)
    )) atoms;
    table
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> (
      if a#equals b then 0
      else match (a#symbol, b#symbol) with
      | ("C", _) -> -2 | (_, "C") -> 2
      | ("H", _) -> -1 | (_, "H") -> 1
      | (_, _) -> a#compare b
    ))
    |> List.fold_left (fun init (a, n) ->
      init ^ a#symbol ^ (if n > 1 then string_of_int n else "")
    ) ""

  method to_string =
    Printf.sprintf "%s: %s" self#name self#formula

  method equals (other: molecule) =
    String.equal self#formula other#formula

end

class water = object
  inherit molecule "Water" Atoms.([new oxygen; new hydrogen; new hydrogen])
end

class carbon_dioxide = object
  inherit molecule "Carbon dioxide" Atoms.([new oxygen; new oxygen; new carbon])
end

class trinitrotoluene = object
  inherit molecule "Trinitrotoluene" Atoms.(
    list (fun () -> new nitrogen) 3
    @ list (fun () -> new hydrogen) 5
    @ list (fun () -> new oxygen) 6
    @ list (fun () -> new carbon) 7
  )
end

class methane = object
  inherit molecule "Methane" Atoms.(new carbon :: list (fun () -> new hydrogen) 4)
end

class aluminum_hydride = object
  inherit molecule "Aluminum Hydride" Atoms.(new aluminum :: list (fun () -> new hydrogen) 3)
end
