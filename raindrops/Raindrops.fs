module Raindrops

let private testEmit divisor sound =
    fun n ->
        if n % divisor = 0 then Some sound else None

let combine (o1: string option) (o2: string option) =
    match o1, o2 with
    | Some s1, Some s2 -> [s1; s2] |> String.concat "" |> Some
    | Some s1, None -> Some s1
    | None, Some s2 -> Some s2
    | None, None -> None
    
let convert (number: int): string =
    let soundsToTest =
        [ testEmit 3 "Pling"
          testEmit 5 "Plang"
          testEmit 7 "Plong" ]
    
    let sounds =
        soundsToTest
        |> List.map (fun emitFor -> emitFor number)
        |> List.fold combine None

    match sounds with
    | None  -> string number
    | Some s -> s