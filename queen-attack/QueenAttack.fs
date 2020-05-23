module QueenAttack

let toOption bool = if bool then Some() else None

let create (position: int * int) =
    let (|ValidPos|_|) input = List.contains input [ 0 .. 7 ] |> toOption
    match position with
    | (ValidPos, ValidPos) -> true
    | _ -> false

let canAttack (queen1: int * int) (queen2: int * int) =
    let (|SameDiagonal|_|) ((x1, y1), (x2, y2)) =
        abs (x1 - x2) = abs (y1 - y2) |> toOption

    let (|SameColumn|_|) ((_,y1), (_,y2)) = y1 = y2 |> toOption

    let (|SameRow|_|) ((x1, _), (x2, _)) = x1 = x2 |> toOption

    match queen1, queen2 with
    | SameRow
    | SameColumn
    | SameDiagonal -> true
    | _ -> false