module Hamming

let charList s = s |> Seq.toList

let actualDistance (strand1: string) (strand2: string) =
    ((charList strand1), (charList strand2))
    ||> List.zip
    |>  List.filter (fun (c1, c2) ->  c1 <> c2)
    |>  List.length

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length <> strand2.Length
    then None
    else Some(actualDistance strand1 strand2)
