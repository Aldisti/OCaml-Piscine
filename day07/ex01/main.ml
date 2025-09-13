
let test_equals m1 m2 =
  Printf.printf "%s == %s -> %s\n" m1#formula m2#formula (m1#equals m2 |> string_of_bool)

let () =
  let w = new Molecules.water in
  let cd = new Molecules.carbon_dioxide in
  let tnt = new Molecules.trinitrotoluene in
  let m = new Molecules.methane in
  let ah = new Molecules.aluminum_hydride in

  print_endline w#to_string;
  print_endline cd#to_string;
  print_endline tnt#to_string;
  print_endline m#to_string;
  print_endline ah#to_string;

  print_newline ();

  test_equals cd m;
  test_equals m cd;
  test_equals tnt tnt;

