
let frequency_count lst =
  List.fold_left (fun init x -> (
    match List.find_opt (fun (y, _) -> x#equals y) init with
    | None -> List.(x, filter x#equals lst |> length) :: init
    | Some _ -> init
  )) [] lst

let unique_list lst =
  let rec internal acc = function
  | [] -> acc
  | e :: t -> internal (
    match List.find_opt (fun x -> e#equals x) acc with
    | None -> e :: acc | Some _ -> acc
  ) t
  in
  internal [] lst


class virtual reaction start result = object (self)
  
  method virtual get_start : (Molecules.molecule * int) list

  method virtual get_result : (Molecules.molecule * int) list

  method virtual balance : reaction

  method is_balanced : bool =
    let sum (molecules: Molecules.molecule list) =
      List.fold_left (fun acc m -> acc + m#mass) 0 molecules
    in
    sum self#start = sum self#result

  method to_string : string =
    let pair_repr (m, i) =
      (if i > 1 then string_of_int i else "") ^ m#formula
    in
    let sl = List.map pair_repr self#get_start in
    let el = List.map pair_repr self#get_result in
    String.concat " + " sl ^ " → " ^ String.concat " + " el

  method private start : Molecules.molecule list = start

  method private result : Molecules.molecule list = result

  method private frequency_count lst =
    List.fold_left (fun init x -> (
      match List.find_opt (fun (y, _) -> x#equals y) init with
      | None -> List.(x, filter x#equals lst |> length) :: init
      | Some _ -> init
    )) [] lst
end

class alkane_combustion ?(result=[]) (alkanes: Alkanes.alkane list) = object (self)
  inherit reaction alkanes result

  method get_start =
    if not self#is_balanced then raise (Failure "Reaction is not balanced.")
    else self#frequency_count self#start

  method get_result =
    if not self#is_balanced then raise (Failure "Reaction is not balanced.")
    else self#frequency_count self#result

  method balance: reaction =
    let rec unpack (lst: ('a * int) list) : 'a list =
      List.fold_left (fun acc (e, n) ->
        List.init n (fun _ -> e) @ acc
      ) [] lst
    in
    if self#is_balanced then
      new alkane_combustion self#start ~result:self#result
    else
      let left = unique_list self#start in
      let left_carbons: int = self#count_atoms (new Atoms.carbon) left in
      let left_hydrogens: int = self#count_atoms (new Atoms.hydrogen) left in
      let left_oxygens: int = self#count_atoms (new Atoms.oxygen) left in
      let odd_case: bool = (left_hydrogens / 2 mod 2) = 1 in
      let right = Molecules.[
        (new carbon_dioxide, left_carbons * (if odd_case then 2 else 1));
        (new water, left_hydrogens / (if odd_case then 1 else 2))
      ] |> unpack in
      let right_oxygens =
        (self#count_atoms (new Atoms.oxygen) right - left_oxygens) / 2
      in
      let left =
        List.init right_oxygens (fun _ -> new Molecules.oxygen)
        @ left @ (if odd_case then [List.hd left] else [])
      in
      new alkane_combustion left ~result:right

  method private count_atoms a lst =
    List.fold_left (fun init m -> ( init +
      match List.find_opt (fun (x, _) -> a#equals x) m#atoms with
      | None -> 0 | Some (_, n) -> n
    )) 0 lst

  method private count_left a =
    self#count_atoms a self#start

end
