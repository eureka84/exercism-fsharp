module CollatzConjecture

let steps (number: int): int option =
    let rec loop n steps =
        match n with
        | 1 -> Some steps
        | _ when n <= 0 -> None
        | _ when n % 2 = 0 -> loop (n / 2) (steps + 1)
        | _ when n % 2 = 1 -> loop (3 * n + 1) (steps + 1)
    loop number 0