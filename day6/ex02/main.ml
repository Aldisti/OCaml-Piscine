
let print_test f_name : unit =
  print_string "\nTesting: ";
  print_endline f_name


let () =

  let dalek = new Dalek.dalek in

  print_test "to_string";
  Printf.printf "dalek: %s\n" dalek#to_string;

  print_test "talk";
  dalek#talk;
  dalek#talk;
  dalek#talk;

  print_test "exterminate";
  dalek#exterminate (new People.people "John");
  Printf.printf "dalek: %s\n" dalek#to_string;

  print_test "die";
  dalek#die;
  Printf.printf "dalek: %s\n" dalek#to_string;

  print_endline "\nLet the War begin...";

  let p1 = new People.people "Joe" in
  let p2 = new People.people "Tom" in
  let doc = new Doctor.doctor "Who" 42 p2 in
  let dal = new Dalek.dalek in

  print_endline dal#to_string;

  dal#exterminate p1;
  print_endline p1#to_string;

  doc#take_damage 50;
  print_endline doc#to_string;

  p2#talk;
  doc#talk;
  dal#talk;

  doc#use_sonic_screwdriver;
  dal#exterminate p2;
  print_endline doc#to_string;

  dal#die;
