module QueenAttack

let create (position: int * int) =
    let isValid pos = pos >=  0 && pos < 8
    isValid (fst position) && isValid (snd position)

let canAttack (queen1: int * int) (queen2: int * int) =
    let sameCol pos1 pos2 = (snd pos1) = (snd pos2)
    let sameRow pos1 pos2 = (fst pos1) = (fst pos2)
    let sameDiag pos1 pos2 = abs (fst pos1 - fst pos2) = abs (snd pos1 - snd pos2)
    sameCol queen1 queen2 || sameRow queen1 queen2 || sameDiag queen1 queen2