
type 'a ft_ref = {mutable contents: 'a}

let return (v: 'a) : 'a ft_ref =
  {contents = v}

let get (r: 'a ft_ref) : 'a =
  r.contents

let set (r: 'a ft_ref) (v: 'a) : unit =
  r.contents <- v

let bind (r: 'a ft_ref) (f: 'a -> 'b ft_ref) : 'b ft_ref =
  f r.contents


let () =
  let r1 = return 0 in
  let r2 = r1 in

  Printf.printf "r1: %d - r2: %d\n" (get r1) (get r2);

  set r2 16;

  Printf.printf "r1: %d - r2: %d\n" (get r1) (get r2);

  let binder = fun x -> return @@ string_of_int @@ x * 3 in

  let r3 = bind r1 binder in

  Printf.printf "r1: %d - r3: %s\n" (get r1) (get r3);
