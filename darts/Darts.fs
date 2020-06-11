module Darts

let private square (x: double) = x * x
let private distanceFromCenter (x: double) (y: double): double =
    sqrt (square x + square y)

let (|OutOfTarget|InsideOuterCircle|InsideMiddleCircle|InsideInnerCircle|) (d: double) =
    match d with
    | _ when (d > 10.0) -> OutOfTarget
    | _ when (d <= 10.0 && d > 5.0) -> InsideOuterCircle
    | _ when (d <= 5.0 && d > 1.0) -> InsideMiddleCircle
    | _ when (d <= 1.0) -> InsideInnerCircle

let score (x: double) (y: double): int =
    match distanceFromCenter x y with
    | OutOfTarget -> 0
    | InsideOuterCircle -> 1
    | InsideMiddleCircle -> 5
    | InsideInnerCircle -> 10
