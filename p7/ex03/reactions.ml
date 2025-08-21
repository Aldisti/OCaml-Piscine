
let frequency_count lst =
  List.fold_left (fun init x -> (
    match List.find_opt (fun (y, _) -> x#equals y) init with
    | None -> List.(x, filter x#equals lst |> length) :: init
    | Some _ -> init
  )) [] lst


class virtual reaction start = object (self)
  
  method virtual get_start : (Molecules.molecule * int) list

  method virtual get_result : (Molecules.molecule * int) list

  method virtual balance : reaction

  method is_balanced : bool =
    let sum molecules =
      List.fold_left (fun init (m, n) -> init + n * m#mass) 0 molecules
    in
    (=) (sum self#get_start) (sum self#get_result)

  method to_string : string =
    let pair_repr (m, i) =
      (if i > 1 then string_of_int i else "") ^ m#formula
    in
    let sl = List.map pair_repr self#get_start in
    let el = List.map pair_repr self#get_result in
    String.concat " + " sl ^ " → " ^ String.concat " + " el

  method private start: Molecules.molecule list = start

end
