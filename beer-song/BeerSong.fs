module BeerSong

let bottles i =
    match i with
    | 1 -> "1 bottle"
    | 0 -> "no more bottles"
    | _ -> sprintf "%i bottles" i

let capitalize (s: string)  =
    s.Substring(0, 1).ToUpper() + 
        ( if s.Length > 1 then s.Substring(1).ToLower() else "");
let firstLine bottlesNumber =
    bottles bottlesNumber
    |> fun x -> sprintf "%s of beer on the wall, %s of beer." x x
    |> capitalize

let secondLine bottlesNumber =
    match bottlesNumber with
    | 0 ->
         "Go to the store and buy some more, 99 bottles of beer on the wall."
    | _ -> 
        let toTakeDown =
            match bottlesNumber with
            | 1 -> "it"
            | _ -> "one"
        let after = bottles (bottlesNumber - 1)
        sprintf "Take %s down and pass it around, %s of beer on the wall." toTakeDown after

let verse bottlesNumber =
    [ firstLine bottlesNumber
      secondLine bottlesNumber ]

let recite (startBottles: int) (takeDown: int) =
    [(startBottles - (takeDown - 1)) .. startBottles]
    |> List.rev
    |> List.map verse
    |> List.reduce (fun acc curr -> acc @ [ "" ] @ curr) 
