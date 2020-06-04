module ArmstrongNumbers

let digitsOf number =
    number |> string |> Seq.map (fun x -> new string ([|x|])) |> Seq.map int

let isArmstrongNumber (number: int): bool =
    let digits = digitsOf number
    let numOfDigits = Seq.length digits

    digits
    |> Seq.map (fun x -> pown x numOfDigits)
    |> Seq.sum
    |> (=) number