
let print r =
    Printf.printf "%s [%b]\n" r#to_string r#is_balanced


let () =

  for i = 1 to 12 do
    Printf.printf "----- %d ------\n" i;
    let alk = new Alkanes.alkane i in
    let r = new Reactions.alkane_combustion [alk] in
    Printf.printf "%s + O2 → CO2 + H2O [%b]\n" alk#formula r#is_balanced;
    try print r with
    | Failure msg -> (
      Printf.printf "Error: %s\n" msg;
      let r = r#balance in
      print r
    )
  done
