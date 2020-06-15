module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int = List.last values

let personalBest (values: int list): int = List.max values    

let personalTopThree (values: int list): int list =
    let sorted = values |> List.sortDescending
    match sorted  with
    | l when List.length l < 3 -> l
    | l -> l |> List.take 3