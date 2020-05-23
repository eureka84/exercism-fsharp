module Raindrops

let private testEmit divisor sound =
    fun n ->
        if n % divisor = 0 then sound else ""

let convert (number: int): string =
    let soundsToTest =
        [ testEmit 3 "Pling"
          testEmit 5 "Plang"
          testEmit 7 "Plong" ]

    let sounds =
        soundsToTest
        |> List.map (fun emitFor -> emitFor number)
        |> List.toSeq
        |> String.concat ""

    match sounds with
    | "" -> string number
    | _ -> sounds
