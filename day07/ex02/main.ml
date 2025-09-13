
let () =
  for i = 1 to 12 do
    let alkane = new Alkanes.alkane i in
    Printf.printf "%d => %s\n" i alkane#to_string
  done;

  let m = new Alkanes.methane in
  let e = new Alkanes.ethane in
  let o = new Alkanes.octane in

  Printf.printf "Methane: %s\n" m#formula;
  Printf.printf "Ethane: %s\n" e#formula;
  Printf.printf "Octane: %s\n" o#formula;

