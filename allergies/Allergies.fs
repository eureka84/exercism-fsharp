module Allergies

open System

type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let EnumValues (enumType: Type): int list =
    let values = Enum.GetValues enumType
    let lb = values.GetLowerBound 0
    let ub = values.GetUpperBound 0
    [ lb .. ub ]
    |> List.map (fun i -> values.GetValue i :?> int)
    |> List.sort
    |> List.rev

let list codedAllergies =
    let rec allergens acc remainingAllergens coded =
        match remainingAllergens, coded with
        | [], _ -> acc
        | _, x when x <=0 -> acc
        | x :: xs, _ ->
            if x > coded then allergens acc xs coded else allergens (x :: acc) xs (coded - x)
    let result  = allergens [] (EnumValues(typeof<Allergen>)) codedAllergies |> List.map enum<Allergen>
    printfn "%A" result
    result

let allergicTo (codedAllergies: int) (allergen: Allergen) =
    list codedAllergies |> List.contains allergen
