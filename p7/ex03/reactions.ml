
let frequency_count lst =
  List.fold_left (fun init x -> (
    match List.find_opt (fun (y, _) -> x#equals y) init with
    | None -> List.(x, filter x#equals lst |> length) :: init
    | Some _ -> init
  )) [] lst


class virtual reaction s e = object (self)
  
  method get_start : (Molecules.molecule * int) list =
    frequency_count s

  method get_end : (Molecules.molecule * int) list =
    frequency_count e

  method virtual balance : reaction

  method is_balanced : bool =
    let sum molecules =
      List.fold_left (fun init (m, n) -> init + n * m#mass) 0 molecules
    in
    (=) (sum self#get_start) (sum self#get_end)

  method to_string : string =
    let pair_repr (m, i) =
      (if i > 1 then string_of_int i else "") ^ m#formula
    in
    let sl = List.map pair_repr self#get_start in
    let el = List.map pair_repr self#get_end in
    String.concat " + " sl ^ " → " ^ String.concat " + " el

end
