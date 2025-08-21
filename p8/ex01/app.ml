
module App = struct
  type project = string * string * int
  let zero : project = ("", "", 0)
  let combine (a, _, a_grade) (b, _, b_grade) =
    let avg = ((a_grade + b_grade) / 2) in
    if avg >= 80 then (a^b, "succeed", avg)
    else (a^b, "failed", avg)

  let fail (x, _, _) = (x, "failed", 0)
  let success (x, _, _) = (x, "succeed", 80)
end



let print_proj (x: App.project) =
  let (x, status, grade) = x in
  Printf.printf "(%s, %s, %d)\n" x status grade

let () =
  let zero = App.zero in
  let math = App.fail ("Math", "", 0) in
  let science = App.success ("Science", "", 0) in
  let history = ("History", "succeed", 90) in

  print_proj zero;
  print_proj math;
  print_proj science;
  print_proj history;
  print_proj (App.combine math history);
  print_proj (App.combine science history);
