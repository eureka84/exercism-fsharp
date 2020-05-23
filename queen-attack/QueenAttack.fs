module QueenAttack

let create (position: int * int) =
    let (|ValidPos|NotValidPos|) input =
        if List.contains input [ 0 .. 7 ] then ValidPos else NotValidPos
    match position with
    | (ValidPos, ValidPos) -> true
    | _ -> false

let canAttack (queen1: int * int) (queen2: int * int) =
    let (|SameDiagonal|_|) (p1, p2) =
        let (x1, y1) = p1
        let (x2, y2) = p2
        if abs (x1 - x2) = abs (y1 - y2) then Some() else None

    let (|SameColumn|_|) (p1, p2) =
        if snd p1 = snd p2 then Some() else None

    let (|SameRow|_|) (p1, p2) =
        if fst p1 = fst p2 then Some() else None

    match queen1, queen2 with
    | SameRow
    | SameColumn
    | SameDiagonal -> true
    | _ -> false