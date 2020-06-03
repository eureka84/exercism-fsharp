module PigLatin

open System.Text.RegularExpressions

let VOWEL_PATTERN = "^([aeiou]|yt|xr).*"
let CONSONANTS_PATTERN = "^(squ|thr|sch|ch|qu|th|rh|[^aeiou])(.*)"

let (|Vowel|_|) input =
    if Regex.Match(input, VOWEL_PATTERN).Success then
        Some()
    else
        None

let (|Consonant|_|) input =
    let m = Regex.Match(input, CONSONANTS_PATTERN)
    if m.Success then
        Some(m.Groups.[1].Value, m.Groups.[2].Value)
    else
        None

let translateWord s =
    match s with
    | Vowel -> s + "ay"
    | Consonant (prefix, postfix) -> postfix + prefix + "ay"
    | _ -> s
    
let translate (input: string) =
    input.Split(" ")
    |> Seq.map translateWord
    |> String.concat " "
