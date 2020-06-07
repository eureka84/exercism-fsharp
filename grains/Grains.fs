module Grains

let square (n: int): Result<uint64, string> =
    if n >= 1 && n <= 64 then Ok(pown 2UL (n - 1)) else Error "square must be between 1 and 64"

let combine (r1: Result<uint64, string>) (r2: Result<uint64, string>): Result<uint64, string> =
    match r1, r2 with
    | Ok s1, Ok s2 -> Ok(s1 + s2)
    | Error _, _ -> r1
    | _, Error _ -> r2

let total: Result<uint64, string> =
    [ 1 .. 64 ]
    |> List.map square
    |> List.reduce combine
