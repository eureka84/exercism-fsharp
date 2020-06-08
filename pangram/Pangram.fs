module Pangram

let isPangram (input: string): bool =
    Set.isSubset (set { 'a' .. 'z' }) (set <| input.ToLower())
        