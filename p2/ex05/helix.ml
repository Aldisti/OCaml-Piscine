
(* TYPES *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {
  pho: phosphate;
  deo: deoxyribose;
  base: nucleobase
}

type helix = nucleotide list

(* FUNCTIONS *)

let generate_nucleotide b =
  let to_base = function
    | 'a' | 'A' -> A
    | 't' | 'T' -> T
    | 'c' | 'C' -> C
    | 'g' | 'G' -> G
    | _ -> None
  in
  {pho="phosphate"; deo="deoxyribose"; base=(to_base b)}


let rec generate_helix n: helix =
  let random_nucleotide =
    match Random.int 4 with
    | 0 -> generate_nucleotide 'a'
    | 1 -> generate_nucleotide 't'
    | 2 -> generate_nucleotide 'c'
    | 3 -> generate_nucleotide 'g'
    | _ -> generate_nucleotide '_'
  in
  if n <= 0 then []
  else random_nucleotide :: generate_helix (n - 1)


let rec helix_to_string (lst: helix) =
  let get_base (n: nucleotide) = n.base in
  let base_to_string = function
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"
  in
  match lst with
  | [] -> ""
  | e :: t -> (get_base e |> base_to_string) ^ helix_to_string t

let rec complementary_helix (lst: helix): helix =
  let complementary_base = function
    | A -> T
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in
  let complementary_nucleo n =
    {n with base=(complementary_base n.base)}
  in
  match lst with
  | [] -> []
  | e :: t -> complementary_nucleo e :: complementary_helix t

(* TEST SUITE *)

let () =
  let get_base = function
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"
  in
  let print_nucleotide nucleo =
    print_string (get_base nucleo.base)
  in
  let rec print_helix = function
    | [] -> ()
    | e :: t -> print_nucleotide e; print_helix t
  in
  let test n =
    let h = (generate_helix n) in
    print_helix h; print_newline ();
    print_endline (helix_to_string h);
    let j = complementary_helix h in
    print_endline (helix_to_string j);
  in
  test 5;
  test 4;
  test 3;
  test (-1)