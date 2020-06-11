module Darts

let private square (x: double) = x * x
let private distanceFromCenter (x: double) (y: double): double = sqrt (square x + square y)

let (|OutOfTarget|InsideOuterCircle|InsideMiddleCircle|InsideInnerCircle|) (x: double, y: double) =
    match distanceFromCenter x y with
    | d when (d <= 10.0 && d > 5.0) -> InsideOuterCircle
    | d when (d <= 5.0 && d > 1.0) -> InsideMiddleCircle
    | d when (d <= 1.0) -> InsideInnerCircle
    | _ -> OutOfTarget

let score (x: double) (y: double): int =
    match (x, y) with
    | OutOfTarget -> 0
    | InsideOuterCircle -> 1
    | InsideMiddleCircle -> 5
    | InsideInnerCircle -> 10
