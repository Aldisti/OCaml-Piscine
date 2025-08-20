
let test_equal a b =
  Printf.printf "%s == %s -> %s\n" a#symbol b#symbol (a#equals b |> string_of_bool)

let () =
  let h = new Atom.hydrogen in
  let c = new Atom.carbon in
  let o = new Atom.oxygen in
  let s = new Atom.sodium in
  let p = new Atom.phosphorus in
  let i = new Atom.iron in

  print_endline h#to_string;
  print_endline c#to_string;
  print_endline o#to_string;
  print_endline s#to_string;
  print_endline p#to_string;
  print_endline i#to_string;

  print_newline ();

  test_equal h c;
  test_equal c h;
  test_equal h h;
