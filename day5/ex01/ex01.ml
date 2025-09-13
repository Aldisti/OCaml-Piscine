
module StringHashtbl = Hashtbl.Make(
  struct
    type t = string
    let equal a b =
      let rec loop acc i =
        if i >= (String.length a) then acc
        else loop (acc && (String.get a i) = (String.get b i)) (i + 1)
      in
      if String.length a <> String.length b then false
      else loop true 0

    let hash s : int = (* Used CRC variant: https://www.cs.hmc.edu/~geoff/classes/hmc.cs070.200101/homework10/hashfuncs.html *)
      let fold_left f init s =
        let len: int = String.length s in
        let rec loop i acc =
          if i >= len then acc
          else loop (i + 1) (f acc (String.get s i))
        in
        loop 0 init
      in
      fold_left (fun init c -> (
        let ho: int = init land 0xf8000000 in
        let init: int = (init lsl 5) lxor (ho lsr 27) in
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
