module Hamming

let actualDistance (strand1: string) (strand2: string) =
    Seq.map2 (<>) strand1 strand2
    |> Seq.filter id
    |> Seq.length

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length <> strand2.Length
    then None
    else Some(actualDistance strand1 strand2)
