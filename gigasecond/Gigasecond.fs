module Gigasecond

open System

let gigaSeconds: TimeSpan = TimeSpan(0, 0, pown 10 9)
let add (beginDate: DateTime) =
    beginDate.Add(gigaSeconds)