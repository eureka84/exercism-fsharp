module ArmstrongNumbers

let digitsOf number =
    number
    |> string
    |> Seq.map (sprintf "%c" >> int)

let isArmstrongNumber (number: int): bool =
    let digits = digitsOf number
    let numOfDigits = Seq.length digits

    digits
    |> Seq.map (fun x -> pown x numOfDigits)
    |> Seq.sum
    |> (=) number
