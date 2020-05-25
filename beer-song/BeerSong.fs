module BeerSong

let bottles i =
    match i with
    | 1 -> "1 bottle"
    | _ -> sprintf "%i bottles" i

let firstLine bottlesNumber =
    match bottlesNumber with
    | 0 -> "No more bottles of beer on the wall, no more bottles of beer."
    | _ -> System.String.Format("{0} of beer on the wall, {0} of beer.", bottles bottlesNumber) 

let secondLine bottlesNumber =
    match bottlesNumber with
    | 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
    | 1 -> "Take it down and pass it around, no more bottles of beer on the wall."
    | _ -> bottles (bottlesNumber - 1) |> sprintf "Take one down and pass it around, %s of beer on the wall."

let verse bottlesNumber =
    [ firstLine bottlesNumber
      secondLine bottlesNumber ]

let recite (startBottles: int) (takeDown: int) =
    [ (startBottles - (takeDown - 1)) .. startBottles ]
    |> List.rev
    |> List.map verse
    |> List.reduce (fun acc curr -> acc @ [ "" ] @ curr)
