module PigLatin

open System.Text.RegularExpressions

type Rule =
    { apply: string -> bool
      transform: string -> string }

let VOWEL_PATTERN = "^([aeiou]|yt|xr).*"
let CONSONANTS_PATTERN = "^(squ|thr|sch|ch|qu|th|rh|[^aeiou])(.*)"

let startsWithAVowel: Rule =
    { apply = fun s -> Regex.Match(s, VOWEL_PATTERN).Success
      transform = fun s -> s + "ay" }

let startsWithAConsonant: Rule =
    { apply = fun s -> Regex.Match(s, CONSONANTS_PATTERN).Success
      transform =
          fun s ->
              let m = Regex.Match(s, CONSONANTS_PATTERN)
              let prefix, postfix = m.Groups.[1].Value, m.Groups.[2].Value
              postfix + prefix + "ay" }

let rules = [ startsWithAVowel; startsWithAConsonant ]

let applyRules s =
    List.tryFind (fun (r: Rule) -> r.apply s) rules
    |> Option.map (fun (r: Rule) -> r.transform s)
    |> Option.defaultValue s
    
let translate (input: string) =
    input.Split(" ")
    |> Array.map applyRules
    |> String.concat " "
