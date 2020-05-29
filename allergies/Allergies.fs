module Allergies

open System

type Allergen =
    | Eggs = 1             // 0 0 0 0 0 0 0 1
    | Peanuts = 2          // 0 0 0 0 0 0 1 0  
    | Shellfish = 4        // 0 0 0 0 0 1 0 0
    | Strawberries = 8     // 0 0 0 0 1 0 0 0 
    | Tomatoes = 16        // 0 0 0 1 0 0 0 0 
    | Chocolate = 32       // 0 0 1 0 0 0 0 0 
    | Pollen = 64          // 0 1 0 0 0 0 0 0 
    | Cats = 128           // 1 0 0 0 0 0 0 0

let allergens =
    Enum.GetValues typeof<Allergen> |> Seq.cast<Allergen>

let isAllergenEncoded codedAllergies allergen =
    int allergen &&& codedAllergies <> 0

let decodeCodedAllergies codedAllergies =
    allergens |> Seq.filter (isAllergenEncoded codedAllergies)

let allergicTo codedAllergies allergen =
    decodeCodedAllergies codedAllergies
    |> Seq.tryFind ((=) allergen)
    |> Option.isSome

let list codedAllergies =
    decodeCodedAllergies codedAllergies |> Seq.toList
