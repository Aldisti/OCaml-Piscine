
(* TYPES *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = {
  pho: phosphate;
  deo: deoxyribose;
  base: nucleobase
}

type helix = nucleotide list
type rna = nucleobase list

(* FUNCTIONS *)

let char_to_base = function
  | 'a' | 'A' -> A
  | 't' | 'T' -> T
  | 'c' | 'C' -> C
  | 'g' | 'G' -> G
  | 'u' | 'U' -> U
  | _ -> None


let base_to_string = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> "None"


let generate_nucleotide b =
  {pho="phosphate"; deo="deoxyribose"; base=(char_to_base b)}


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


let rec generate_rna (lst: helix) : rna =
  let to_compl_base n =
    match n.base with
    | A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in
  match lst with
  | [] -> []
  | e :: t -> to_compl_base e :: generate_rna t



(* TEST SUITE *)

let () =
  Random.self_init ();
  let rec print_rna = function
    | [] -> ()
    | e :: t -> print_string (base_to_string e); print_rna t
  in
  let test n =
    let dna = generate_helix n in
    let rna = generate_rna dna in
    print_endline (helix_to_string dna);
    print_rna rna; print_newline ()
  in
  test 10;
  test 4;
  test 3;
  test (-42)