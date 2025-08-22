
module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module Make (FB: FRACTIONNAL_BITS) : FIXED = struct
  type t = int
  let bits = FB.bits
  let of_float (f: float) : t =
    let dec: float = f -. Float.trunc f in
    let dec: int = Float.((dec *. (1 lsl bits |> of_int)) |> round |> to_int) in
    (Float.to_int f lsl bits) + dec
  let of_int (n: int) : t = n lsl bits
  let to_float (n: t) : float = (* thanks gpanico <3 *)
    let num: int = n lsr bits in
    let tmp: float = float_of_int (n - (num lsl bits)) in
    (float_of_int num) +. (tmp /. (float_of_int (1 lsl bits)))
  let to_int (n: t) : int = n lsr bits
  let to_string (n: t) : string = to_float n |> string_of_float
  let zero : t = 0
  let one : t = of_int 1
  let succ (n: t) : t = n + 1
  let pred (n: t) : t = n - 1
  let min (a: t) (b: t) : t = if a < b then a else b
  let max (a: t) (b: t) : t = if a > b then a else b
  let gth (a: t) (b: t) : bool = a > b
  let lth (a: t) (b: t) : bool = a < b
  let gte (a: t) (b: t) : bool = a >= b
  let lte (a: t) (b: t) : bool = a <= b
  let eqp (a: t) (b: t) : bool = a == b (** physical equality *)
  let eqs (a: t) (b: t) : bool = a = b (** structural equality *)
  let add (a: t) (b: t) : t = a + b
  let sub (a: t) (b: t) : t = a - b
  let mul (a: t) (b: t) : t = a * b lsr bits
  let div (a: t) (b: t) : t = (a lsl bits) / b
  let foreach (start: t) (stop: t) (f: t -> unit) : unit =
    let s = ref start in
    while !s <= stop do
      f !s; s := succ !s
    done
end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));

  let x = Fixed8.of_int 42 in
  let y = Fixed8.div x (Fixed8.of_int 2) in
  let z = Fixed8.of_int 42 in

  Printf.printf "x: %s - y: %s - z: %s\n" (Fixed8.to_string x) (Fixed8.to_string y) (Fixed8.to_string z);

  let test_compare ?(n1=x) ?(n2=y) f s =
    Printf.printf "%s %s %s) => %s\n" (Fixed8.to_string n1) s (Fixed8.to_string n2) (string_of_bool (f n1 n2))
  in
  let test_min_max ?(n1=x) ?(n2=y) f s =
    Fixed8.(Printf.printf "%s(%s, %s) => %s\n" s (to_string n1) (to_string n2) (f n1 n2 |> to_string))
  in
  let test_oper ?(n1=x) ?(n2=y) f s =
    Fixed8.(Printf.printf "%s %s %s = %s\n" (to_string n1) s (to_string n2) (f n1 n2 |> to_string))
  in

  print_endline "Less than (or equal to):";
  test_compare Fixed8.lth "<";
  test_compare Fixed8.lte "<=";
  test_compare Fixed8.lth "<" ~n2:z;
  test_compare Fixed8.lte "<=" ~n2:z;
  print_endline "Greater then (or equal to):";
  test_compare Fixed8.gth ">";
  test_compare Fixed8.gte ">=";
  test_compare Fixed8.gth ">" ~n2:z;
  test_compare Fixed8.gte ">=" ~n2:z;
  print_endline "Equal to:";
  test_compare Fixed8.eqp "=";
  test_compare Fixed8.eqs "==";
  test_compare Fixed8.eqp "=" ~n2:z;
  test_compare Fixed8.eqs "==" ~n2:z;
  print_endline "Min and max:";
  test_min_max Fixed8.min "min";
  test_min_max Fixed8.max "max";
  test_min_max Fixed8.min "min" ~n2:z;
  test_min_max Fixed8.max "max" ~n2:z;

  let x = Fixed8.(x |> pred |> pred |> pred |> pred |> pred) in
  let y = Fixed8.of_float 21.5 in
  Fixed8.(Printf.printf "x: %s - y: %s\n" (to_string x) (to_string y));

  print_endline "Operations:";
  test_oper Fixed8.add "+" ~n1:x ~n2:y;
  test_oper Fixed8.sub "-" ~n1:x ~n2:y;
  test_oper Fixed8.mul "*" ~n1:x ~n2:y;
  test_oper Fixed8.div "/" ~n1:x ~n2:y;

  Printf.printf "(int)x => %d\n" (Fixed8.to_int x)
