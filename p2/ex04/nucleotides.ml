
type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {
  pho: phosphate;
  deo: deoxyribose;
  base: nucleobase
}

let generate_nucleotide b =
  let to_base = function
    | 'a' | 'A' -> A
    | 't' | 'T' -> T
    | 'c' | 'C' -> C
    | 'g' | 'G' -> G
    | _ -> None
  in
  {pho="phosphate"; deo="deoxyribose"; base=(to_base b)}

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
    Printf.printf "{pho: %s, deo: %s, base: %s}\n" nucleo.pho nucleo.deo (get_base nucleo.base)
  in
  let test c =
    print_char c; print_string " -> ";
    print_nucleotide (generate_nucleotide c);
  in
  test 'a';
  test 'c';
  test 'g';
  test 't';
  test 'z';