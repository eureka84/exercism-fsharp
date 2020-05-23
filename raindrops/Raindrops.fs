module Raindrops

let private testEmit divisor sound =
    fun n ->
        if n % divisor = 0 then Some sound else None
        
let sequence (l:list<option<'T>>): option<list<'T>> =
    let folder (el: option<'T>) (acc: option<list<'T>>): option<list<'T>> =
          let addEl (curr: list<'T>) = el |> Option.map (fun s -> s::curr)
          acc |> Option.bind addEl         
    List.foldBack folder l (Some [])

let convert (number: int): string =
    let soundsToTest =
        [ testEmit 3 "Pling"
          testEmit 5 "Plang"
          testEmit 7 "Plong" ]

    let maybeAListOfSounds =
        soundsToTest
        |> List.map (fun emitFor -> emitFor number)
        |> List.filter Option.isSome
        |> sequence      

    match maybeAListOfSounds with
    | None
    | Some [] -> string number
    | Some sounds -> sounds |> List.toSeq |> String.concat "" 
