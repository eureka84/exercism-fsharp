module Darts

let private square (x: double) = x * x
let private distanceFromZero (x: double) (y: double): double =
    sqrt (square x + square y)

let private toOption (b: bool) =
    if b then Some() else None

let score (x: double) (y: double): int =
    let (|OutOfTarget|_|) (d: double) = (d > 10.0) |> toOption
    let (|InsideOuterCircle|_|) (d: double) = (d <= 10.0 && d > 5.0) |> toOption
    let (|InsideMiddleCircle|_|) (d: double) = (d <= 5.0 && d > 1.0) |> toOption
    let (|InsideInnerCircle|_|) (d: double) = (d <= 1.0) |> toOption

    match distanceFromZero x y with
    | OutOfTarget -> 0
    | InsideOuterCircle -> 1
    | InsideMiddleCircle -> 5
    | InsideInnerCircle -> 10
