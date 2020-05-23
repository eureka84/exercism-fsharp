module QueenAttack

let create (position: int * int) =
    let isValid pos = pos >=  0 && pos < 8
    isValid (fst position) && isValid (snd position)

let canAttack (queen1: int * int) (queen2: int * int) =
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    let sameCol = y1 = y2
    let sameRow = x1 = x2
    let sameDiag = abs (y1 - y2) = abs (x1 - x2)
    sameCol || sameRow || sameDiag 