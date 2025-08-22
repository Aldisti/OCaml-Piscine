
let () =
  let p = new People.people ("John") in

  Printf.printf "p = %s\n" p#to_string;
  p#talk;
  p#die
