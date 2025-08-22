
let print_people lst =
  let rec internal = function
    | [] -> print_endline "]"
    | [e] -> print_string e#to_string; internal []
    | e :: t -> print_string e#to_string; print_string ", "; internal t
  in
  print_string "["; internal lst

let print_doctors lst =
  let rec internal = function
    | [] -> print_endline "]"
    | [e] -> print_string e#to_string; internal []
    | e :: t -> print_string e#to_string; print_string ", "; internal t
  in
  print_string "["; internal lst

let print_daleks lst =
  let rec internal = function
    | [] -> print_endline "]"
    | [e] -> print_string e#to_string; internal []
    | e :: t -> print_string e#to_string; print_string ", "; internal t
  in
  print_string "["; internal lst

let () =

  print_endline "Generating data:";
  let p1 = new People.people "John" in
  let p2 = new People.people "Tom" in
  let p3 = new People.people "Ben" in
  let d1 = new Doctor.doctor "Who" 42 p1 in
  let d2 = new Doctor.doctor "Who" 21 p2 in
  let dal1 = new Dalek.dalek in
  let dal2 = new Dalek.dalek in

  print_endline "\nTesting army of People";
  let army = new Army.army in

  army#add p1; army#add p2; army#add p3;
  print_people army#get_members;

  army#delete;
  print_people army#get_members;

  army#delete; army#delete; army#delete;
  print_people army#get_members;

  print_endline "\nTesting army of Doctors";
  let army = new Army.army in

  army#add d1; army#add d2;
  print_doctors army#get_members;

  army#delete;
  print_doctors army#get_members;

  print_endline "\nTesting army of Daleks";
  let army = new Army.army in

  army#add dal1;
  print_daleks army#get_members;

  army#delete; army#add dal2;
  print_daleks army#get_members;
