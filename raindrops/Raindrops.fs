module Raindrops

let private testEmit divisor sound =
    fun n ->
        if n % divisor = 0 then Some sound else None

let convert (number: int): string =
    let soundsToTest =
        [ testEmit 3 "Pling"
          testEmit 5 "Plang"
          testEmit 7 "Plong" ]

    let sounds =
        soundsToTest
        |> List.map (fun emitFor -> emitFor number)
        |> List.filter Option.isSome
        |> List.map Option.get

    match sounds with
    | [] -> string number
    | _ ->
        sounds
        |> List.toSeq
        |> String.concat ""
