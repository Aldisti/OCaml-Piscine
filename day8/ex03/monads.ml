
module Try = struct
  type 'a t = Success of 'a | Failure of exn

  let return (x: 'a) : 'a t = Success x

  let bind (x: 'a t) (f: 'a -> 'b t) : 'b t =
    match x with
    | Success e -> (try f e with | ex -> Failure ex)
    | Failure e -> Failure e

  let recover (x: 'a t) (f: exn -> 'a t) : 'a t =
    match x with
    | Success _ -> x
    | Failure e -> f e

  let filter (x: 'a t) (f: 'a -> bool) : 'a t =
    match x with
    | Failure _ -> x
    | Success e -> if f e then x else Failure (Failure "error")

  let flatten (x: 'a t t) : 'a t =
    match x with
    | Failure e -> Failure e
    | Success e -> (
      match e with
      | Success r -> Success r
      | Failure r -> Failure r
    )

end

(* TEST *)

let () =
  let check (t: 'a Try.t) : unit =
    match t with
    | Success e -> Printf.printf "Number: %d\n" e
    | Failure _ -> print_endline "Error"
  in

  print_endline "Test: Try.return";
  let t = Try.return 42 in
  check t;

  print_endline "Test: Try.bind";
  let fail = Try.bind t (fun x -> Try.return (x / 0)) in
  check fail;

  let success = Try.bind t (fun x -> Try.return (x / 2)) in
  check success;

  print_endline "Test: Try.recover";
  let recover = Try.recover fail (fun _ -> Try.return 1) in
  check recover;

  print_endline "Test: Try.filter";
  let success_filter = Try.filter success (fun x -> x > 1) in
  check success_filter;

  let fail_filter = Try.filter fail (fun x -> x > 1) in
  check fail_filter;

  print_endline "Test: Try.flatten";
  let fail_flatten = fail_filter |> Try.return |> Try.flatten in
  check fail_flatten;

  let success_flatten = success |> Try.return |> Try.flatten in
  check success_flatten;
