module Pangram

open System.Text.RegularExpressions

let isPangram (input: string): bool =
    Regex.Replace(input.ToLower(), "[^a-z]", "")
    |> Seq.distinct
    |> Seq.length
    |> (=) 26
        