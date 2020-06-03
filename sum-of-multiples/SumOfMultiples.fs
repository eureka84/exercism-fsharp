module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let factors = numbers |> List.filter (fun x -> x <> 0)
    let isXDivisibleByY x y = x % y = 0
    let isDivisibleByOneOfFactors x = List.exists (isXDivisibleByY x) factors 
    [ 1 .. (upperBound - 1) ]
    |> List.filter isDivisibleByOneOfFactors 
    |> List.sum
