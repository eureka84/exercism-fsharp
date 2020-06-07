module Gigasecond

open System

let gigaSeconds = pown 10.0 9
let add (beginDate: DateTime) =
    beginDate.AddSeconds(gigaSeconds)