
module StringHashtbl = Hashtbl.Make(
  struct
    type t = string
    let equal = String.equal
    let hash s : int = (* Used CRC variant: https://www.cs.hmc.edu/~geoff/classes/hmc.cs070.200101/homework10/hashfuncs.html *)
      String.fold_left (fun init c -> (
        let ho = init land 0xf8000000 in
        let init = init lsl 5 in
        let init = init lxor (ho lsr 27) in
        init lxor (int_of_char c)
      )) 0 s
  end
)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
