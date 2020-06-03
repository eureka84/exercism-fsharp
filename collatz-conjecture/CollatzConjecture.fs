module CollatzConjecture

let steps (number: int): int option =
    let rec computeSteps n steps =
        match n with
        | 1 -> steps
        | _ when n % 2 = 0 -> computeSteps (n / 2) (steps + 1)
        | _ when n % 2 = 1 -> computeSteps (3 * n + 1) (steps + 1)
    match number with
    | n when n <= 0 -> None
    | n -> Some <| computeSteps n 0