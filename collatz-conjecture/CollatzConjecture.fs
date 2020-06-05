module CollatzConjecture

let (|Even|Odd|) input =
    if input % 2 =0 then Even else Odd

let steps (number: int): int option =
    let rec computeSteps n steps =
        match n with
        | 1 -> steps
        | Even -> computeSteps (n / 2) (steps + 1)
        | Odd -> computeSteps (3 * n + 1) (steps + 1)
    match number with
    | n when n <= 0 -> None
    | n -> Some <| computeSteps n 0