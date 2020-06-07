module Hamming

let private charList s = s |> Seq.toList

let actualDistance (strand1: string) (strand2: string) =
    ((charList strand1), (charList strand2))
    ||> List.zip
    |>  List.sumBy (fun (c1, c2) -> if c1 <> c2 then 1 else 0)

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length <> strand2.Length
    then None
    else Some(actualDistance strand1 strand2)
