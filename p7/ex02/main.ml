
let () =
  for i = 1 to 12 do
    let alkane = new Alkanes.alkane i in
    Printf.printf "%d => %s\n" i alkane#to_string
  done
