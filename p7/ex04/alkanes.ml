
class alkane (n: int) = object
  inherit Molecules.molecule (
    match n with
    |  1 -> "Methane" |  2 -> "Ethane"
    |  3 -> "Propane" |  4 -> "Butane"
    |  5 -> "Pentane" |  6 -> "Hexane"
    |  7 -> "Heptane" |  8 -> "Octane"
    |  9 -> "Nonane" | 10 -> "Decane"
    | 11 -> "Undecane" | 12 -> "Dodecane"
    | _ -> ""
  ) Atoms.(
    list (fun () -> new carbon) n
    @ list (fun () -> new hydrogen) (2 * n + 2)
  )
end
