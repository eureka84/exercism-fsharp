module Etl

let private toPairsList (score: int, letters: char list): (char * int) list =
    letters
    |> List.map System.Char.ToLower 
    |> List.map (fun letter -> (letter, score))
let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
    scoresWithLetters
    |> Map.toSeq
    |> Seq.map toPairsList
    |> Seq.concat
    |> Map.ofSeq
