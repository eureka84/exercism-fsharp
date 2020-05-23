module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec innerMap incoming output = 
        match incoming with
        | [] -> output
        | x::xs ->  innerMap xs (func x :: output) 
    innerMap input [] |> List.rev 