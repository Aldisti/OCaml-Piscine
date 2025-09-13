
let () =
  let p = new People.people "Robin" in

  print_endline "\nTest: initializer";
  let d = new Doctor.doctor "Who" 42 p in

  print_endline "\nTest: to_string";
  Printf.printf "d = %s\n" d#to_string;

  print_endline "\nTest: talk";
  d#talk;

  print_endline "\nTest: travel_in_time";
  d#travel_in_time 2025 1950;
  Printf.printf "d = %s\n" d#to_string;

  print_endline "\nTest: take_damage";
  d#take_damage 42;
  Printf.printf "d = %s\n" d#to_string;

  print_endline "\nTest: use_sonic_screwdriver";
  d#use_sonic_screwdriver;
  Printf.printf "d = %s\n" d#to_string;

